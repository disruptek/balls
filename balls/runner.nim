import std/algorithm
import std/atomics
import std/hashes
import std/heapqueue
import std/lists
import std/options
import std/os
import std/osproc
import std/sequtils
import std/sets
import std/strutils
import std/tables
import std/times

import pkg/insideout
import pkg/cps
import pkg/ups/sanitize

import balls/spec
import balls/semaphores
import balls/style
import balls/tabouli
import balls

const
  ballsFailFast* {.booldefine.} = true ##
  ## if true, quit early on a test failure
  ballsUseValgrind* {.booldefine.} = true ##
  ## if true, attempt to use valgrind in preference to asan/tsan

type
  Backend* = enum  ## backends that we test
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

  Analyzer* = enum  ## analysis tools
    Execution  = "execute"
    ASanitizer = "asan"
    TSanitizer = "tsan"
    Valgrind   = "memcheck"
    Helgrind   = "helgrind"
    DataRacer  = "drd"

  Matrix* = OrderedTable[Profile, StatusKind] ##
  ## the Matrix collects test results in the order they are obtained

  Profile* = object ##
  ## the Profile defines compilation settings for a single test invocation
    an*: Analyzer
    be*: Backend
    opt*: Optimizer
    gc*: MemModel
    fn*: string

const
  anCompilerInvocation* = {Execution, ASanitizer, TSanitizer}
  anValgrindInvocation* = {Valgrind, Helgrind, DataRacer}

proc hash*(p: Profile): Hash =
  ## Two Profiles that `hash` identically share a test result in the Matrix.
  var h: Hash = 0
  h = h !& hash(p.an)
  h = h !& hash(p.be)
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
  "$#: $# $# $#" % [ short p.fn, $p.be, $p.gc, $p.opt ]

template cmper(f: untyped) {.dirty.} =
  result = system.cmp(a.`f`, b.`f`)
  if result != 0:
    return

proc cmp*(a, b: Profile): int =
  ## Compare Profiles `a`, `b` for the purposes of test ordering.
  ## Note that this comparison does not measure test filename.
  cmper be
  cmper gc
  cmper opt
  cmper an

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

proc nearby(p: Profile): (string, int, int, int) =
  result = (p.fn.shortPath, ord p.be, ord p.opt, ord p.an)

proc parameters*(): seq[string] =
  ## ladies and gentlemen, the command-line arguments
  for i in 1..paramCount():
    result.add paramStr(i)

proc parameter*(parameters: seq[string]; switch: string): bool =
  ## true if the command-line switch is present in the arguments;
  ## this necessarily performs a style-insensitive comparison
  parameters.anyIt:
    0 == cmpIgnoreStyle(it, switch)

proc parameter*(switch: string): bool =
  ## true if the command-line switch is present in the command-line arguments;
  ## this necessarily performs a style-insensitive comparison
  parameter(parameters(), switch)

template makeSpecifier(tipe: typedesc[enum]; prefixes: openArray[string]): untyped =
  ## makes filters which sus out specified values corresponding to compiler
  ## options relevant to our matrix of test profiles, as provided via cli.
  proc `specified tipe`*(params: openArray[string]): seq[tipe] {.used.} =
    let params = map(params, toLowerAscii)
    for value in tipe:
      for prefix in prefixes.items:
        if (prefix & $value).toLowerAscii in params:
          result.add value

  proc `filtered tipe`*(params: seq[string]): seq[string] {.used.} =
    var params = params
    for value in tipe:
      for prefix in prefixes:
        params =
          params.filterIt:
            it.toLowerAscii != (prefix & $value).toLowerAscii
    result = params

makeSpecifier(MemModel, ["--gc:", "--mm:"])
makeSpecifier(Backend, ["-b:", "--backend:"])
makeSpecifier(Optimizer, ["-d:", "--define:"])

proc toSet[T: int8 | int16 | enum | uint8 | uint16 | char](ss: openArray[T]): set[T] =
  for item in ss.items:
    result.incl item

iterator rowPermutations(matrix: Matrix; p: Profile): Profile =
  ## emit the profile permutations appropriate for this row
  var p = p
  for mm in MemModel:
    p.gc = mm
    if p.gc == vm:
      p.be = e
    yield p
  p.be = js
  p.gc = vm
  yield p

proc matrixTable*(matrix: Matrix): string =
  ## Render the `matrix` as a table.
  var matrix = matrix
  var tab = Tabouli()
  tab.headers = @["nim-" & NimVersion, "", ""]
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
    var row =
      case p.an
      of Execution:
        @[p.fn.shortPath, $p.be, $p.opt]
      else:
        @[p.fn.shortPath, $p.be, $p.an]

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
  var omit = @["Cc", "Link", "Conf", "Processing", "Exec", "Name",
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
    omit.add ["ObservableStores", "UnreachableCode"]
  for warning in omit.items:
    result.add " --warning[$#]=off" % [ warning ]

let ci* = getEnv("GITHUB_ACTIONS", "false") == "true"
var matrix*: Matrix
# set some default matrix members (profiles)
var opt* = {
  debug: @["--debuginfo", "--stackTrace:on", "--excessiveStackTrace:on"],
  release: @["--define:release", "--stackTrace:on",
             "--excessiveStackTrace:on"],
  danger: @["--define:danger"],
}.toTable

# use the optimizations specified on the command-line
var specifiedOpt = specifiedOptimizer(parameters()).toSet
if specifiedOpt == {}:
  # no optimizations were specified; omit debug
  opt.del debug
else:
  # if optimizations were specified, remove any (not) specified
  let removeOpts = toSeq(Optimizer.items).toSet - specifiedOpt
  for optimizer in removeOpts.items:
    opt.del optimizer

# use the backends specified on the command-line
var be* = specifiedBackend(parameters()).toSet
# and if those are omitted, we'll select sensible defaults
if be == {}:
  be.incl c                     # always test c
  if ci:
    be.incl cpp                 # on ci, add cpp
    be.incl js                  # on ci, add js
    be.incl e                   # on ci, add nimscript

# use the memory models specified on the command-line
let specifiedMM = specifiedMemModel(parameters()).toSet
var gc* = specifiedMM
# and if those are omitted, we'll select a single default
if gc == {}:
  gc.incl arc
  # danger is no longer required to pass, so this is a useful place to
  # produce some extra warnings and test future defaults
  if danger in opt:
    opt[danger].add "--panics:on"
    opt[danger].add "--experimental:strictFuncs"
  if ci:
    gc.incl refc              # on ci, add refc
    gc.incl markAndSweep      # on ci, add markAndSweep
    if arc in gc:
      gc.incl orc             # on ci, add orc if arc is available

# if the nimscript or javascript backends are specified, enable the vm
if {e, js} * be != {}:
  gc.incl vm

# options common to all profiles
var defaults* = @["""--path=".""""]  # work around early nim behavior

when compileOption"threads":
  defaults.add "--parallelBuild:1"

if ci:
  # force incremental off so as not to get confused by a config file
  defaults.add "--incremental:off"

proc cache*(p: Profile): string =
  ## come up with a unique cache directory according to where you'd like
  ## to thread your compilations under ci or local environments.
  let opt =
    case p.an
    of ASanitizer, TSanitizer:
      $p.an                      # asan/tsan are essentially optimizations
    else:
      $p.opt                     # valgrind/helgrind/drd reuse executables
  let suffix =
    if false and ci:
      # we don't want to skip subsequent tests which differ only by filename
      "$#.$#.$#" % [ $p.be, opt, $p.gc ]
    else:
      "$#.$#.$#.$#" % [ $hash(p.fn), $p.be, opt, $p.gc ]
  result = getTempDir()
  result = result / "balls-nimcache-$#-$#" % [ suffix, $getCurrentProcessId() ]

proc runCommandLine(cmd: string; args: openArray[string]): (string, int) =
  const options = {poEvalCommand, poStdErrToStdOut, poUsePath}
  var process = startProcess(cmd, options = options)
  # ... process.outputStream or process.outputHandle
  close process

proc attempt*(cmd: string): (string, int) =
  ## attempt execution of a random command; returns output and exit code
  try:
    when compileOption"threads":
      result = execCmdEx cmd
    else:
      result = ("", execCmd cmd)
  except OSError as e:
    let output = "$1: $2" % [ $e.name, e.msg ]
    result = (output, 1)

proc checkpoint*(matrix: Matrix) =
  checkpoint:
    "\n" & matrixTable(matrix) & "\n"

proc output*(p: Profile): string =
  ## return the output filename for the build
  when compileOption"threads":
    "$#_$#_$#" % [ short(p.fn), $getThreadId(), $hash(p) ]
  else:
    "$#_$#" % [ short(p.fn), $hash(p) ]

proc options*(p: Profile): seq[string] =
  result = defaults & opt[p.opt]

  # grab the user's overrides provided on the command-line
  var params = parameters()

  # filter out any "specified" options we've already consumed
  params = filteredMemModel params
  params = filteredOptimizer params
  params = filteredBackend params

  # and otherwise pass those options on to the compiler
  result.add params

  # specify the nimcache directory
  result.add "--nimCache:" & p.cache

  # specify the output filename
  result.add "--out:\"$#\"" % [ p.output ]

  # use the nimcache for our output directory
  result.add "--outdir:\"$#\"" % [ p.cache ]   # early nims dunno $nimcache

  if p.be == js:
    # add --define:nodejs on js backend so that getCurrentDir() works
    result.add "--define:nodejs"

  when false:
    # nimscript doesn't use a --run
    if p.be != e:
      # don't run compile-only tests
      if not result.parameter "--compileOnly":
        result.add "--run"

  # use goto exceptions only in c
  if p.be == c:
    result.add "--exceptions:goto"

  template installDebugInfo {.dirty.} =
    # adjust the debugging symbols for asan/tsan builds
    for switch in ["--debuginfo", "--debugger:native"]:
      if switch notin result:
        result.add switch

  case p.an
  of ASanitizer:
    result.add """--passC:"-fsanitize=address""""
    result.add """--passL:"-fsanitize=address""""
    installDebugInfo()
  of TSanitizer:
    result.add """--passC:"-fsanitize=thread""""
    result.add """--passL:"-fsanitize=thread""""
    installDebugInfo()
  else:
    discard

let useValgrind = ballsUseValgrind and "" != findExe"valgrind"

proc nonsensical*(p: Profile): bool =
  ## certain profiles need not be attempted
  if p.gc == vm and p.be notin {js, e}:
    true
  elif p.be in {js, e} and p.gc != vm:
    true
  elif p.fn == changeFileExt(p.fn, "nims") and p.gc != vm:
    true
  elif p.an in anValgrindInvocation and not useValgrind:
    true
  elif p.an in {ASanitizer, TSanitizer} and useValgrind:
    true
  elif p.gc == vm and p.an in anValgrindInvocation:
    true
  elif p.an != Execution and p.opt notin {danger}:
    true
  elif p.an != Execution and p.gc notin {arc, orc}:
    true
  else:
    false

iterator compilerCommandLine(p: Profile; withHints = false): string =
  ## compose the interesting parts of the compiler invocation
  let pattern =
    if p.gc == vm:
      "nim $1 $3"
    else:
      "nim $1 --mm:$2 $3"
  var result = pattern % [$p.be, $p.gc, join(p.options, " ")]
  if withHints:
    # determine which hints to include
    let hs = hints(p, ci)
    # add the hints into the invocation ahead of the filename
    result &= " " & hs
  # return the command-line with the filename for c+p reasons
  result &= " " & p.fn
  # yield the compilation command
  yield result

  if not p.options.parameter("--compileOnly"):
    # invoke the output
    case p.be
    of js:
      yield "node " & (p.cache / p.output)
    of e:
      yield "nim e " & p.fn
    else:
      yield p.cache / p.output

iterator valgrindCommandLine(p: Profile; withHints = false): string =
  ## compose the interesting parts of the valgrind invocation
  let executable =
    block:
      # make sure the executable has been built successfully
      var compilation = p
      compilation.an = Execution
      for compilation in compilation.compilerCommandLine(withHints = withHints):
        yield compilation
        # omit any execution from the "compiler" commandLine iterator
        break
      compilation.cache / compilation.output

  var result = @["valgrind"]
  result.add: "--error-exitcode=255"  # for zevv
  result.add: "--tool=" & (if p.an == DataRacer: "drd" else: $p.an)
  case p.an
  of Valgrind:
    result.add: ["--leak-check=full", "--show-leak-kinds=all"]
    result.add: ["--track-origins=yes", "--max-threads=50000"]
  of Helgrind:
    result.add: ["--max-threads=50000"]
  of DataRacer:
    result.add: ["--first-race-only=yes", "--join-list-vol=50000"]
    result.add: ["--report-signal-unlocked=no"]
  else:
    discard
  result.add: executable
  yield join(result, " ")

iterator commandLine*(p: Profile; withHints = false): string =
  ## compose the interesting parts of the process invocations
  case p.an
  of anCompilerInvocation:
    for command in p.compilerCommandLine(withHints = withHints):
      yield command
  of anValgrindInvocation:
    for command in p.valgrindCommandLine(withHints = withHints):
      yield command

proc perform*(p: Profile): StatusKind =
  ## Run a single Profile `p` and return its StatusKind.
  assert not p.nonsensical
  let hintFree = p.commandLine(withHints = false).toSeq
  let commands = p.commandLine(withHints = true).toSeq
  for index in 0..commands.high:
    let (output, code) = attempt commands[index]
    case code
    of 0:
      result = Pass
    else:
      result = Fail
      for hintless in hintFree[0..index].items:
        checkpoint "$ " & hintless
      checkpoint output
      break

proc `[]=`*(matrix: var Matrix; p: Profile; s: StatusKind) =
  ## emit the matrix report whenever it changes
  tables.`[]=`(matrix, p, s)
  checkpoint matrix

proc shouldPass*(p: Profile): bool =
  ## true if the test should pass according to current nim climate
  # neither cpp nor js nor nimscript backends are required to work
  if p.be notin {cpp, js, e}:
    # danger builds can fail; they include experimental features
    if p.opt notin {danger}:
      result = true

var cores: Semaphore
initSemaphore(cores, countProcessors())

proc shouldCrash(matrix: var Matrix; p: Profile): bool =
  # let the user deny crashes
  if not ballsFailFast: return false

  if matrix[p] > Part and p.shouldPass:
    for command in p.commandLine:
      checkpoint command
    setBallsResult int(matrix[p] > Part)
    # before we fail the ci, run a debug test for shits and grins
    var n = p
    n.opt = debug
    if n notin matrix:      # a safer solution
      if debug in opt:      # do we even know how?
        discard perform n
        matrix[n] = Info
    let (s, code) = execCmdEx "nim --version"
    if code == 0:
      checkpoint "failure; compiler:"
      checkpoint s
    else:
      checkpoint "failure; unable to determine compiler version"
    result = true

type
  Update = ref object of Continuation
    profile: Profile
    status: StatusKind

proc setup(c: Update; p: Profile; s: StatusKind): Update {.cpsMagic.} =
  c.profile = p
  c.status = s
  result = c

proc statusUpdate(monitor: Mailbox[Update]; profile: Profile;
                  status: StatusKind) {.cps: Update.} =
  setup profile, status
  goto monitor

var pleaseCrash: Atomic[bool]

proc matrixMonitor(box: Mailbox[Update]) {.cps: Continuation.} =
  ## debounce status updates received from test attempts
  var matrix: Matrix
  var mail: Update
  var dirty = false
  while true:
    if not box.tryRecv mail:
      # there's nothing waiting; dump the matrix?
      if dirty:
        checkpoint matrix
        dirty = false
      mail = recv box
    if dismissed mail:
      break
    else:
      # update the matrix with the profile->status
      tables.`[]=`(matrix, mail.profile, mail.status)
      if mail.status != Wait:
        # check to see if we should crash
        if matrix.shouldCrash(mail.profile):
          checkpoint matrix
          echo "CRASH"
          pleaseCrash.store true
        else:
          dirty = true
      # send control wherever it needs to go next
      discard trampoline Continuation(mail)

proc runBatch(box: Mailbox[Continuation]; monitor: Mailbox[Update];
              profiles: seq[Profile]): StatusKind {.cps: Continuation.} =
  ## run a series of profiles in order
  var queue = profiles.toHeapQueue

  # mark them as waiting
  var profiles = profiles
  while profiles.len > 0:
    statusUpdate(monitor, pop(profiles), Wait)
    goto box

  while queue.len > 0:
    goto box
    let profile = pop queue
    if result >= Skip: # prior failure; skip the remainder
      result = Skip
    else:              # grab a core, mark it running, and run it
      withSemaphore cores:
        statusUpdate(monitor, profile, Runs)
        goto box
        result = perform profile
    statusUpdate(monitor, profile, result)
  # the batch is complete; drop a thread
  box.send nil.Continuation

const MonitorService = whelp matrixMonitor
proc perform*(matrix: var Matrix; profiles: seq[Profile]) =
  ## concurrent testing of the provided profiles
  if profiles.len == 0:
    return

  # batch the profiles according to their cache
  var batches: Table[Hash, seq[Profile]]
  for profile in profiles.items:
    let cache = hash(cache profile)
    if cache in batches:
      batches[cache].add profile
    else:
      batches[cache] = @[profile]

  # make a pool of workers and send them the batches
  var workers = newMailbox[Continuation]()
  var pool = newPool(ContinuationWaiter, workers, batches.len)

  # setup a debouncing matrix monitor
  var monitor: Runtime[Continuation, Update]
  let updates = monitor.spawn MonitorService
  defer: quit monitor

  for profiles in batches.values:
    workers.send:
      whelp runBatch(workers, updates, profiles)

  # drain the pool
  while not pool.isEmpty:
    drain pool
    if load pleaseCrash:
      echo "QUIT1!"
      quit 1
  if load pleaseCrash:
    echo "QUIT2!"
    quit 1

proc profiles*(fn: string): seq[Profile] =
  ## Produce profiles for a given test filename.
  var profile: Profile
  profile.fn = fn
  for opt in opt.keys:
    profile.opt = opt
    for gc in gc.items:
      profile.gc = gc
      for be in be.items:
        profile.be = be
        for an in Analyzer.items:
          profile.an = an
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
