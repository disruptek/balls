import std/times
import std/options
import std/tables
import std/os
import std/osproc
import std/hashes
import std/algorithm
import std/strutils
import std/sequtils
import std/heapqueue

when not compileOption"threads":
  {.error: "balls currently requires threads".}
import std/rlocks

import ups/sanitize
import sync/semaphore

import balls/spec
import balls/style
import balls/tabouli
import balls

const
  ballsFailFast* {.booldefine.} = true ##
  ## if true, quit early on a test failure

type
  Compiler* = enum  ## backends that we test
    c
    cpp
    js
    e

  Optimizer* = enum ## optimization modes that we test
    debug
    release
    danger

  MemModel* = enum  ## memory managers that we test
    refc
    markAndSweep
    arc
    orc
    vm

  Matrix* = OrderedTable[Profile, StatusKind] ##
  ## the Matrix collects test results in the order they are obtained

  Profile* = object ##
  ## the Profile defines compilation settings for a single test invocation
    cp*: Compiler
    opt*: Optimizer
    gc*: MemModel
    fn*: string

  Payload = object
    cache: ptr RLock
    profile: Profile
    status: ptr StatusKind         # ptr is necessary for non-arc nims

  TestThread = Thread[Payload]

proc hash*(p: Profile): Hash =
  ## Two Profiles that `hash` identically share a test result in the Matrix.
  var h: Hash = 0
  h = h !& hash(p.cp)
  h = h !& hash(p.opt)
  h = h !& hash(p.gc)
  h = h !& hash(p.fn)
  #h = h !& hash(p.options)
  result = !$h

proc short(fn: string): string =
  extractFilename changeFileExt(fn, "")

proc shortPath(fn: string): string =
  fn.parentDir.lastPathPart / fn.short

proc `$`(p: Profile): string =
  "$#: $# $# $#" % [ short p.fn, $p.cp, $p.gc, $p.opt ]

template cmper(f: untyped) {.dirty.} =
  result = system.cmp(a.`f`, b.`f`)
  if result != 0:
    return

proc cmp*(a, b: Profile): int =
  ## Compare Profiles `a`, `b` for the purposes of test ordering.
  ## Note that this comparison does not measure test filename.
  cmper cp
  cmper gc
  cmper opt

proc `<`*(a, b: Profile): bool {.used.} =
  ## Compare Profiles `a`, `b` for the purposes of test ordering.
  ## Note that this comparison does not measure test filename.
  cmp(a, b) == -1

proc `==`*(a, b: Profile): bool {.used.} =
  ## Compare Profiles `a`, `b` for the purposes of test ordering.
  ## Note that this comparison does not measure test filename.
  cmp(a, b) == 0

proc contains*(matrix: Matrix; p: Profile): bool =
  ## A test result of `None` or `Skip` effectively does not count as being
  ## present in the test `matrix`.
  matrix.getOrDefault(p, None) notin {None}

proc nearby(p: Profile): (string, int, int) =
  result = (p.fn.shortPath, ord p.cp, ord p.opt)

when false:
  proc useEarlyRow(matrix: Matrix; p: Profile): bool =
    if p.cp in {js, e}:
      var p = p
      p.gc = default MemModel
      p.cp = default Compiler
      result = p in matrix

iterator rowPermutations(matrix: Matrix; p: Profile): Profile =
  ## emit the profile permutations appropriate for this row
  var p = p
  for mm in MemModel:
    p.gc = mm
    if p.gc == vm:
      p.cp = e
    yield p
  p.cp = js
  p.gc = vm
  yield p

proc matrixTable*(matrix: Matrix): string =
  ## Render the `matrix` as a table.
  var matrix = matrix
  var tab = Tabouli()
  tab.headers = @["nim-" & NimVersion, "cp", "opt"]
  tab.freeze = len tab.headers
  for mm in MemModel:
    tab.headers.add:
      if mm == markAndSweep:
        "m&s"
      else:
        $mm
  tab.headers.add $js

  # while the matrix has members,
  while matrix.len > 0:
    # reorder the remaining profiles by their display order
    #let profiles = toSeq(matrix.keys).sortedByIt(it.nearby)
    # this is dumb for nim-1.[02] reasons
    var profiles = toSeq matrix.keys
    proc byProximity(a, b: auto): int = cmp(a.nearby, b.nearby)
    profiles.sort(byProximity, Ascending)

    # the first profile in that list is the best one to show next
    var p = profiles[0]

    # compose a row's prefix labels in a lame way
    var row = @[p.fn.shortPath, $p.cp, $p.opt]

    # then iterate over the memory models and consume any results
    for p in rowPermutations(matrix, p):
      # pull the run out of the matrix if possible
      # (we can't use pop|take whatfer nim-1.0 reasons)
      if p in matrix:
        let status = matrix[p]
        row.add:
          if useColor():
            $statusStyles[status] & $status
          else:
            $status
      else:
        row.add " "
      matrix.del p        # we have to scrub all matching profiles thusly
    if row[3..^1].join(" ").strip() != "":   # omit rows without any status
      tab.rows.add row    # we're done with this row; add it to the table

  # pass the length of StatusKind.None; this corresponds to the width
  # of the other StatusKind values, in characters, which is 1 for bland
  # values and 2 for wide emojis
  result = render(tab, size = len $None)

proc hints*(p: Profile; ci: bool): string =
  ## Compute `--hint` and `--warning` flags as appropriate given Profile
  ## `p`, `ci` status, and compile-time Nim version information.
  var omit = @["Cc", "Link", "Conf", "Processing", "Exec",
               "XDeclaredButNotUsed"]
  if ci or p.opt notin {danger}:
    # ignore performance warnings outside of local danger builds
    omit.add "Performance"
  for hint in omit.items:
    result.add " --hint[$#]=off" % [ hint ]

  ## compute --warning(s) as appropriate
  omit = @[]
  if ci:
    # remove spam from ci logs
    omit.add ["UnusedImport", "ProveInit", "CaseTransition"]
    when (NimMajor, NimMinor) >= (1, 2):
      omit.add "ObservableStores"
    when (NimMajor, NimMinor) >= (1, 4):
      omit.add "UnreachableCode"
  for warning in omit.items:
    result.add " --warning[$#]=off" % [ warning ]

let ci = getEnv("GITHUB_ACTIONS", "false") == "true"
var matrix: Matrix
# set some default matrix members (profiles)
var opt = {
  debug: @["--debuginfo", "--stackTrace:on", "--excessiveStackTrace:on"],
  release: @["--define:release", "--stackTrace:on",
             "--excessiveStackTrace:on"],
  danger: @["--define:danger"],
}.toTable
var cp = @[c]
# the default gc varies with version
var gc: set[MemModel]
when (NimMajor, NimMinor) >= (1, 2):
  gc.incl arc
  # danger is no longer required to pass, so this is a useful place to
  # produce some extra warnings and test future defaults
  when (NimMajor, NimMinor) >= (1, 5):
    opt[danger].add "--panics:on"
    opt[danger].add "--experimental:strictFuncs"
    when false:
      #
      # removed because if i cannot make it work, i can hardly expect you to
      #
      if ci:
        # notnil is too slow to run locally
        opt[danger].add "--experimental:strictNotNil"
else:
  gc.incl refc
# options common to all profiles
var defaults = @["""--path=".""""]  # work around early nim behavior

when compileOption"threads":
  defaults.add "--parallelBuild:1"

if (NimMajor, NimMinor) >= (1, 8):
  # always use IC if it's available
  defaults.add "--incremental:on"
elif ci:
  # otherwise, force rebuild only on CI
  if not compileOption"threads":
    # and only outside threads
    defaults.add "--forceBuild:on"
  when (NimMajor, NimMinor) >= (1, 5):
    # force incremental off so as not to get confused by a config file
    defaults.add "--incremental:off"

# remote ci expands the matrix
if ci:
  cp.add cpp                  # add cpp
  cp.add js                   # add js
  cp.add e                    # add nimscript
  gc.incl refc                # add refc
  gc.incl markAndSweep        # add markAndSweep
  if arc in gc:               # add orc if arc is available
    when (NimMajor, NimMinor) >= (1, 4):  # but 1.2 has infinite loops!
      gc.incl orc
  if js in cp:
    gc.incl vm
else:
  # do a danger build locally so we can check time/space; omit release
  opt.del release

proc cache(p: Profile): string =
  ## come up with a unique cache directory according to where you'd like
  ## to thread your compilations under ci or local environments.
  ## the thinking here is that local tests vary by filename while the ci
  ## tests vary primarily by garbage collector.
  when compileOption"threads":
    var suffix =
      if ci:
        "$#.$#.$#" % [ $p.cp, $p.opt, $p.gc ]
      else:
        "$#.$#.$#" % [ $hash(p.fn), $p.cp, $p.opt ]
  else:
    var suffix = $p.cp  # no threads; use a unique cache for each backend

  result = getTempDir()
  result = result / "balls-nimcache-$#-$#" % [ suffix, $getCurrentProcessId() ]

proc attempt(cmd: string; display = false): int =
  ## attempt execution of a random command; returns the exit code
  try:
    when compileOption"threads":
      var output: string
      (output, result) = execCmdEx cmd
      if result != 0 or display:
        noclobber:
          checkpoint "$ " & cmd
          checkpoint output
    else:
      checkpoint "$ " & cmd
      result = execCmd cmd
  except OSError as e:
    checkpoint "$1: $2" % [ $e.name, e.msg ]
    result = 1

proc checkpoint(matrix: Matrix) =
  checkpoint:
    "\n" & matrixTable(matrix) & "\n"

proc options(p: Profile): seq[string] =
  result = defaults & opt[p.opt]

  # add in any command-line arguments
  for index in 1 .. paramCount():
    result.add paramStr(index)

  # specify the nimcache directory
  result.add "--nimCache:" & p.cache

  # use an unlikely filename for output
  let output =
    when compileOption"threads":
      "$#_$#_$#" % [ short(p.fn), $getThreadId(), $hash(p) ]
    else:
      "$#_$#" % [ short(p.fn), $hash(p) ]
  result.add "--out:\"$#\"" % [ output ]

  # use the nimcache for our output directory
  result.add "--outdir:\"$#\"" % [ p.cache ]   # early nims dunno $nimcache

  # turn off panics on 1.4 because writeStackTrace breaks js builds
  if p.cp == js:
    when (NimMajor, NimMinor) == (1, 4):
      keepItIf(result, it != "--panics:on")

    # add --define:nodejs on js backend so that getCurrentDir() works
    result.add "--define:nodejs"

  # nimscript doesn't use a --run
  if p.cp != e:
    # don't run compile-only tests
    if "--compileOnly" notin result:
      result.add "--run"

  # turn off sinkInference on 1.2 builds because it breaks VM code
  when (NimMajor, NimMinor) == (1, 2):
    result.add "--sinkInference:off"

  when (NimMajor, NimMinor) >= (1, 5):
    # use goto exceptions only in c
    if p.cp == c:
      result.add "--exceptions:goto"

func nonsensical(p: Profile): bool =
  ## certain profiles need not be attempted
  if p.gc == vm and p.cp notin {js, e}:
    true
  elif p.cp in {js, e} and p.gc != vm:
    true
  elif p.fn == changeFileExt(p.fn, "nims") and p.gc != vm:
    true
  else:
    false

proc run*(p: Profile; withHints = false): string =
  ## compose the interesting parts of the compiler invocation
  let pattern =
    if p.gc == vm:
      "nim $1 $3"
    else:
      "nim $1 --gc:$2 $3"

  result = pattern % [$p.cp, $p.gc, join(p.options, " ")]

  if withHints:
    # determine which hints to include
    let hs = hints(p, ci)
    # add the hints into the invocation ahead of the filename
    result &= " " & hs

  # return the command-line with the filename for c+p reasons
  result &= " " & p.fn

proc perform*(p: Profile): StatusKind =
  ## Run a single Profile `p` and return its StatusKind.
  assert not p.nonsensical
  result =
    # we'll display danger output when run locally
    case attempt(p.run(withHints = true),
                 display = false) # p.opt == danger and not ci)
    of 0: Pass
    else: Fail

proc `[]=`(matrix: var Matrix; p: Profile; s: StatusKind) =
  ## emit the matrix report whenever it changes
  tables.`[]=`(matrix, p, s)
  checkpoint matrix

proc shouldPass(p: Profile): bool =
  ## true if the test should pass according to current nim climate
  const MajorMinor = $NimMajor & "." & $NimMinor
  # neither cpp or js or nimscript backends are required to work
  if p.cp notin {cpp, js, e}:
    # danger builds can fail; they include experimental features
    if p.opt notin {danger}:
      result = true
      case MajorMinor
      of "1.4", "1.3", "1.2", "1.1", "1.0":
        # arc/orc have fatal bugs on 1.4
        if p.gc >= arc:
          result = false
      else:
        discard

var cores: Semaphore
cores.init countProcessors()

proc performThreaded(p: Payload) {.thread.} =
  ## run perform, but do it in a thread with a lock on the compilation cache
  {.gcsafe.}:
    p.status[] = Wait
    withRLock p.cache[]:
      wait cores
      try:
        p.status[] = Runs
        p.status[] = perform p.profile
      finally:
        # indicate that we're done with the core
        signal cores

  # we don't conditionally raise anymore because we don't join threads, so
  # we cannot catch it easily in the parent thread; hence we rely upon the
  # parent to measure the status.  no big deal.
  #
  when false:
    case p.status[]
    of Pass:
      discard
    else:
      if p.profile.shouldPass:
        let message = "failure: " & $p.profile & "\n" & p.profile.run
        when ballsFailFast:
          # if we should crash, go ahead and raise
          raise CatchableError.newException message
        else:
          # or just emit an error message
          checkpoint message

proc lesserTestFailed(matrix: Matrix; profile: Profile): bool =
  ## true if a lesser test already failed, meaning we can
  ## skip the provided profile safely
  template dominated(e: typedesc[enum]; field: untyped) {.dirty.} =
    for value in e.items:
      if value < profile.field:
        var test = profile
        test.field = value
        if test in matrix and matrix[test] > Part:
          # a tiny hack to ensure that vm dominance is separate
          when e is MemModel:
            if (test.gc == vm) == (profile.gc == vm):
              return true
          else:
            return true

  dominated(Optimizer, opt)
  #dominated(Compiler, cp)
  dominated(MemModel, gc)

proc countRunning(threads: seq[TestThread]): int =
  ## a countIt for early nims 🙄
  for thread in threads.items:
    if thread.running:
      inc result

proc perform*(matrix: var Matrix; profs: seq[Profile]) =
  ## Try to run `profs` and fail early if you can.
  var threads = newSeqOfCap[TestThread](profs.len)
  var locks = initTable[string, RLock](2)
  #var profiles = profs.toHeapQueue   # only works in later nims
  var profiles: HeapQueue[Profile]
  for p in profs.items:
    profiles.push p

  # we need to enlarge the matrix table and pre-allocate the status
  # values so that we can pass them as pointers to the threads
  for p in profs.items:
    # safely re-entrant, and []= would print the table... 😉
    discard matrix.hasKeyOrPut(p, None)
    # allocate and initialize locks for the nimcaches while we're at it
    if p.cache notin locks:
      locks[p.cache] = default RLock
      initRLock locks[p.cache]

  try:
    while profiles.len > 0:
      var p = profiles.pop
      if p notin matrix:
        if lesserTestFailed(matrix, p):
          matrix[p] = Skip
        else:
          setLen(threads, threads.len + 1)
          createThread threads[^1], performThreaded:
            Payload(cache: addr locks[p.cache], profile: p,
                    status: addr matrix[p])

    var count = threads.len
    while threads.anyIt it.running:
      sleep 250
      let running = countRunning threads
      if running != count:
        checkpoint matrix
        count = running
  except CatchableError as e:
    checkpoint e.msg

  for p in matrix.keys:
    if matrix[p] > Part and p.shouldPass:
      checkpoint p.run
      setBallsResult int(matrix[p] > Part)
      # before we fail the ci, run a debug test for shits and grins
      var n = p
      n.opt = debug
      if n notin matrix:      # a safer solution
        discard perform n
        matrix[n] = Info
      let (s, code) = execCmdEx "nim --version"
      if code == 0:
        checkpoint "failure; compiler:"
        checkpoint s
      else:
        checkpoint "failure; unable to determine compiler version"
      quit 1

proc profiles*(fn: string): seq[Profile] =
  ## Produce profiles for a given test filename.
  for opt in opt.keys:
    if not ci or opt > debug:         # omit debug on ci
      for gc in gc.items:
        for cp in cp.items:
          var profile = Profile(fn: fn, gc: gc, cp: cp, opt: opt)
          if not profile.nonsensical:
            result.add profile

proc ordered*(directory: string; testsOnly = true): seq[string] =
  ## Order a directory tree of test files usefully; set `testsOnly`
  ## for rigid "must start with a t and end with .nim" behavior.  If
  ## `testsOnly` is set, the search is recursive.
  if not directory.dirExists: return @[]
  if testsOnly:
    # collect the filenames recursively, but only .nim
    for test in walkDirRec(directory, yieldFilter = {pcFile, pcLinkToFile}):
      if test.extractFilename.startsWith("t") and test.endsWith(".nim"):
        result.add test
  else:
    # don't recurse; just collect files, but also consume .nims
    for kind, test in walkDir directory:
      if test.endsWith(".nim") or test.endsWith(".nims"):
        result.add test

  # if we're not in strict mode,
  if not testsOnly:
    type
      # just documentation for now...
      Sig {.used.} = enum
        Zero = "no files match the provided extension"
        One  = "one file matches and it shares the name of the project"
        Many = "multiple files exist for the given extension"

    proc matching(among: seq[string]; pro: string): seq[string] =
      ## pluck out files from `among` which match the project name
      const
        useCaps = true
      let proj = sanitizeIdentifier(pro, capsOkay = useCaps)
      if proj.isNone:
        # the current directory isn't a sane identifier 🙄
        return @[]
      else:
        for file in among.items:
          let splat = file.extractFilename.splitFile
          let name = sanitizeIdentifier(splat.name, capsOkay = useCaps)
          if name.isSome:
            if name.get == proj.get:
              result.add file

    let proj = extractFilename getCurrentDir()
    var promatches = matching(result, proj)
    sort promatches
    for ext in [".nim", ".nims"]:
      # these are files that match the given extension
      var files = filterIt(result, it.splitFile.ext == ext)

      # collect the instances of these that share the same import name
      var matches = matching(files, proj)
      sort matches

      # some of these scenarios will cause us to skip changing the result,
      # while others will cause us to replace the result list with one file
      if files.len == 0:                                    # Zero
        continue
      elif matches.len == 1:                                # One
        discard
      elif matches.len > 0 and matches == promatches:       # One
        # XXX: for now, we ignore x.nims in x.(nims|nim)
        discard
        #continue
      else:                                                 # Many
        continue

      # we want a single file; the best of the project-named files
      result = @[matches[0]]
      break

  # sort them by age, recently-changed first
  proc age(path: string): Time =
    getFileInfo(path, followSymlink = true).lastWriteTime
  proc byAge(a, b: string): int = system.cmp(a.age, b.age)
  result.sort(byAge, Descending)

proc main*(directory: string; fallback = false) =
  ## Run each test in the `directory` in a useful order; set `fallback` to
  ## `true` to fall back to a loose search in the current directory for
  ## testable code.
  var tests: seq[string]
  # first check the supplied directory
  tests = ordered directory
  try:
    # if there are no tests in the directory,
    if tests.len == 0:
      # try to find something good to run in the current directory
      tests = ordered(getCurrentDir(), testsOnly = false)
    if tests.len == 0:
      checkpoint "couldn't find any tests to run; that's good, right?"
      quit 0
  except OSError as e:
    checkpoint "bad news about the current directory... it's gone?"
    checkpoint "the os says `$#`" % [ e.msg ]
    quit 1

  # generate profiles for the ordered inputs
  var profiles: seq[Profile]
  for test in tests.items:
    profiles &= test.profiles

  try:
    # run the profiles
    matrix.perform profiles

  finally:
    # remove any cache directories
    for p in matrix.keys:
      removeDir p.cache
