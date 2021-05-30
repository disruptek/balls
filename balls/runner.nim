import std/times
import std/options
import std/tables
import std/os
import std/osproc
import std/hashes
import std/algorithm
import std/strutils
import std/sequtils
import std/deques
import std/math

import ups/sanitize

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
    ran*: string
    fn*: string

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

proc labels(p: Profile): (string, string, string) =
  (p.fn.shortPath, $p.cp, $p.opt)

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

  # while the matrix has members,
  while matrix.len > 0:
    # reorder the remaining profiles by their display order
    #let profiles = toSeq(matrix.keys).sortedByIt(it.labels)
    # this is dumb for nim-1.[02] reasons
    var profiles = toSeq matrix.keys
    proc byLabels(a, b: auto): int = cmp(a, b)
    profiles.sort(byLabels, Ascending)

    # the first profile in that list is the best one to show next
    var p = profiles[0]

    # compose a row's prefix labels in a lame way
    var row = @[p.labels[0], p.labels[1], p.labels[2]]

    # then iterate over the memory models and consume any results
    for mm in MemModel:
      p.gc = mm
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
    opt[danger].add "--exceptions:goto"
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

if (NimMajor, NimMinor) >= (1, 6):
  # always use IC if it's available
  defaults.add "--incremental:on"
elif ci:
  # otherwise, force rebuild only on CI
  defaults.add "--forceBuild:on"
  when (NimMajor, NimMinor) >= (1, 5):
    # force incremental off so as not to get confused by a config file
    defaults.add "--incremental:off"

# remote ci expands the matrix
if ci:
  cp.add cpp                  # add cpp
  cp.add js                   # add js
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

proc attempt(cmd: string): int =
  ## attempt execution of a random command; returns the exit code
  checkpoint "$ " & cmd
  try:
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

  # turn off panics on 1.4 because writeStackTrace breaks js builds
  if p.cp == js:
    when (NimMajor, NimMinor) == (1, 4):
      keepItIf(result, it != "--panics:on")

    # add --define:nodejs on js backend so that getCurrentDir() works
    result.add "--define:nodejs"

  # don't run compile-only tests
  if "--compileOnly" notin result:
    result.add "--run"

  # turn off sinkInference on 1.2 builds because it breaks VM code
  when (NimMajor, NimMinor) == (1, 2):
    result.add "--sinkInference:off"

proc perform*(p: var Profile): StatusKind =
  ## Run a single Profile `p` and return its StatusKind.
  let pattern =
    if p.gc == vm:
      "nim $1 $3"
    else:
      "nim $1 --gc:$2 $3"
  # we also use it to determine which hints to include
  let hs = hints(p, ci)

  # compose the remainder of the command-line
  var run = pattern % [$p.cp, $p.gc, join(p.options, " ")]

  # store the command-line with the filename for c+p reasons
  p.ran = run & " " & p.fn

  # add the hints into the invocation ahead of the filename
  run.add hs & " " & p.fn

  # certain profiles don't even get attempted
  if p.gc == vm and p.cp != js:
    checkpoint "(skipping NimScript test on $#)" % [ $p.gc ]
    result = None
  elif p.cp == js and p.gc != vm:
    checkpoint "(skipping $# test on $#)" % [ $p.gc, $p.cp ]
    result = None
  else:
    # run it and return the result
    let code = attempt run
    case code
    of 0:
      result = Pass
    else:
      result = Fail

proc `[]=`(matrix: var Matrix; p: Profile; s: StatusKind) =
  ## emit the matrix report whenever it changes
  tables.`[]=`(matrix, p, s)
  checkpoint matrix

proc lesserTestFailed(matrix: Matrix; p: Profile): bool =
  ## true if a lesser test already failed, meaning we can
  ## skip the provided profile safely
  template dominated(e: typedesc[enum]; f: untyped) {.dirty.} =
    for f in e.items:
      if f < p.f:
        var p = p
        p.f = f
        if p in matrix and matrix[p] > Part:
          return true

  dominated(Optimizer, opt)
  #dominated(Compiler, cp)
  dominated(MemModel, gc)

proc perform*(matrix: var Matrix; profs: seq[Profile]) =
  ## Try to run `profiles` and fail early if you can.
  # sort the profiles and put them in a deque for easier consumption
  var profiles = initDeque[Profile](nextPowerOfTwo profs.len)
  for p in sorted(profs, cmp).items:         # order the profiles
    profiles.addLast p
  while profiles.len > 0:
    var p = profiles.popFirst
    matrix[p] =
      if lesserTestFailed(matrix, p):
        Skip
      else:
        perform p

    const MajorMinor = $NimMajor & "." & $NimMinor
    if matrix[p] > Part:
      case MajorMinor
      of "1.4":
        if p.gc > orc:
          continue
      of "1.2":
        if p.gc > arc:
          continue
      else:
        discard
      checkpoint p.ran
      checkpoint "failed; compiler:"
      flushStderr()   # hope we beat the compiler's --version
      discard execCmd "nim --version"
      # don't quit when run locally; just keep chugging away
      if ci and ballsFailFast:
        # neither cpp or js backends are expected to work 100% of the time
        if p.cp notin {cpp, js}:
          # arc and orc are still too unreliable to demand successful runs
          if p.gc notin {arc, orc}:
            # danger builds can fail; they include experimental features
            if p.opt notin {danger, debug}:
              setBallsResult int(matrix[p] > Part)
              # before we fail the ci, run a debug test for shits and grins
              var n = p
              n.opt = debug
              if n notin matrix:      # a safer solution
                discard perform n
                matrix[n] = Info
              quit 1

proc profiles*(fn: string): seq[Profile] =
  ## Produce profiles for a given test filename.
  for opt in opt.keys:
    if not ci or opt > debug:         # omit debug on ci
      for gc in gc.items:
        for cp in cp.items:
          var profile = Profile(fn: fn, gc: gc, cp: cp, opt: opt)
          result.add profile

proc ordered*(directory: string; testsOnly = true): seq[string] =
  ## Order a directory tree of test files usefully; set `testsOnly`
  ## for rigid "must start with a t and end with .nim" behavior.  If
  ## `testsOnly` is set, the search is recursive.
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

  # first check the supplied directory
  var tests = ordered directory
  # if there are no tests in the directory,
  if tests.len == 0:
    # try to find something good to run in the current directory
    tests = ordered(getCurrentDir(), testsOnly = false)

  var profiles: seq[Profile]
  # generate profiles for the ordered inputs
  for test in tests.items:
    profiles = concat(profiles, test.profiles)

  # polish the profile order to smoke the project faster
  sort profiles

  try:
    # run the profiles
    matrix.perform profiles

  finally:
    # remove any cache directories
    for p in matrix.keys:
      removeDir p.cache
