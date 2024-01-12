import std/algorithm
import std/atomics
import std/hashes
import std/heapqueue
import std/lists
import std/locks
import std/monotimes
import std/options
import std/os
import std/osproc
import std/sequtils
import std/sets
import std/streams
import std/strutils
import std/tables
import std/times

import pkg/insideout
import pkg/cps
import pkg/ups/sanitize
import pkg/ups/config
import pkg/ups/paths

import balls/spec
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
  "$#: $# $# $# $#" % [ short p.fn, $p.be, $p.gc, $p.opt, $p.an ]

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
          if useColor:
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

proc hints*(p: Profile; ci: bool): seq[string] =
  ## Compute `--hint` and `--warning` flags as appropriate given Profile
  ## `p`, `ci` status, and compile-time Nim version information.
  var omit = @["Cc", "Link", "Conf", "Processing", "Exec", "Name",
               "XDeclaredButNotUsed"]
  if ci or p.opt notin {danger}:
    # ignore performance warnings outside of local danger builds
    omit.add "Performance"
  for hint in omit.items:
    result.add "--hint[$#]=off" % [ hint ]

  ## compute --warning(s) as appropriate
  omit = @[]
  if ci:
    # remove spam from ci logs
    omit.add ["UnusedImport", "ProveInit", "CaseTransition"]
    omit.add ["ObservableStores", "UnreachableCode", "BareExcept"]
  for warning in omit.items:
    result.add "--warning[$#]=off" % [ warning ]

let ci* = getEnv("GITHUB_ACTIONS", "false") == "true"
# set some default matrix members (profiles)
var opt* = {
  debug: @["--debuginfo:on",
           "--lineTrace:on", "--stackTrace:on", "--excessiveStackTrace:on"],
  release: @["--define:release",
             "--lineTrace:on", "--stackTrace:on", "--excessiveStackTrace:on"],
  danger: @["--define:danger"]
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
# and if those are omitted, we'll select reasonable defaults
if gc == {}:
  if ci:
    gc = {arc, orc}
  else:
    gc = {arc}

# if the nimscript or javascript backends are specified, enable the vm
if {e, js} * be != {}:
  gc.incl vm

# options common to all profiles
var defaults* = @["--incremental:off", "--panics:on", "--parallelBuild:1"]

proc cache*(p: Profile): string =
  ## come up with a unique cache directory according to where you'd like
  ## to thread your compilations under ci or local environments.

  # FIXME: this gets replaced with a config sniff?

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

var startProcessLock: Lock
initLock startProcessLock

proc runCommandLine(commandArguments: openArray[string]): (string, int) =
  ## run a command with arguments; returns (output, exitcode)
  const options = {poStdErrToStdOut}
  let cmd = commandArguments[0]
  let arguments: seq[string] =
    if commandArguments.len > 1:
      commandArguments[1..^1]
    else:
      @[]
  if not cmd.fileExists:
    raise OSError.newException "file not found: " & cmd
  else:
    var process: Process
    withLock startProcessLock:
      process = startProcess(cmd, args = arguments, options = options)
    try:
      close process.inputStream
      let (lines, code) = process.readLines()
      result = (lines.join("\n"), code)
    finally:
      close process

proc attempt*(command: seq[string]): (string, int) =
  ## attempt execution of a random command; returns output and exit code
  try:
    result = runCommandLine command
  except OSError as e:
    let output = "$1: $2" % [ $e.name, e.msg ]
    result = (output, 1)

proc checkpoint*(matrix: Matrix) =
  if matrix.len > 0:
    checkpoint:
      "\n" & matrixTable(matrix) & "\n"

proc output*(p: Profile): string =
  ## return the output filename for the build

  # FIXME: this gets replaced with a config sniff?

  "$#_$#_$#" % [ short(p.fn), $getThreadId(), $hash(p) ]

proc assetOptions*(p: Profile): seq[string] =
  ## by setting the cache and output paths, we can find our assets

  # FIXME: this gets replaced with a config sniff?

  if p.be != e:
    # specify the nimcache directory
    result.add "--nimCache:" & p.cache

    # specify the output filename
    result.add "--out:" & p.output

    # use the nimcache for our output directory
    result.add "--outDir:" & p.cache

proc options*(p: Profile): seq[string] =
  result = defaults & opt[p.opt]

  # add a memory management option if appropriate
  if p.gc != vm:
    when defined(isNimSkull):
      result.add "--gc:" & $p.gc
    else:
      result.add "--mm:" & $p.gc

  # grab the user's overrides provided on the command-line
  var params = parameters()

  # filter out any "specified" options we've already consumed
  params = filteredMemModel params
  params = filteredOptimizer params
  params = filteredBackend params

  # and otherwise pass those options on to the compiler
  result.add params

  if p.be == js:
    # add --define:nodejs on js backend so that getCurrentDir() works
    result.add "--define:nodejs"

  # use goto exceptions only in c
  if p.be == c:
    result.add "--exceptions:goto"

  if p.an != Execution:
    # adjust the debugging symbols for analysis builds
    const Switches = [
      "--define:useMalloc",
      "--debuginfo",
      # Enable line directives to map C lines back to Nim
      "--linedir",
      # Enable frame pointers for better backtraces
      "--passC:'-fno-omit-frame-pointer'",
      "--passC:'-mno-omit-leaf-frame-pointer'"
    ]
    for switch in Switches:
      if switch notin result:
        result.add switch

  case p.an
  of ASanitizer:
    result.add """--passC:"-fsanitize=address""""
    result.add """--passL:"-fsanitize=address""""
  of TSanitizer:
    result.add """--passC:"-fsanitize=thread""""
    result.add """--passL:"-fsanitize=thread""""
  else:
    discard

let nimExecutable = findExe"nim"
let valgrindExecutable = findExe"valgrind"
let useValgrind = "" != valgrindExecutable and
  parseBool(getEnv("BALLS_VALGRIND", $ballsUseValgrind))

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
  elif p.an != Execution and p.options.parameter("--compileOnly"):
    true
  else:
    false

iterator compilerCommandLine(p: Profile; withHints = false): seq[string] =
  ## compose the interesting parts of the compiler invocation
  var result = @[nimExecutable]
  if p.be == e and p.options.parameter("--compileOnly"):
    result.add "check"                # we can only "check" the VM backend
  else:
    result.add $p.be                  # add the backend, eg. `c`, `js`, `e`
  result.add p.options                # add other options for this profile

  if withHints:                       # toggled so we can omit in an echo
    result.add p.assetOptions         # add --nimCache, --out, --outDir
    result.add hints(p, ci)           # add Cc, Link, Conf, Processing, etc.
    when ballsDry:
      result.add "--define:ballsDry"  # dry runners beget dry tests

  result.add p.fn                     # add the filename for c+p reasons

  yield result                        # run the compiler against the input

  if not p.options.parameter("--compileOnly"):
    case p.be
    of js:
      yield @[findExe"node", p.cache / p.output]   # invoke the output
    of e:
      discard                                      # (already consumed)
    else:
      yield @[p.cache / p.output]                  # invoke the output

iterator valgrindCommandLine(p: Profile; withHints = false): seq[string] =
  ## compose the interesting parts of the valgrind invocation
  let executable =
    block:
      # make sure the executable has been built successfully
      var compilation = p
      for compilation in compilation.compilerCommandLine(withHints = withHints):
        yield compilation
        # omit any execution from the "compiler" commandLine iterator
        break
      compilation.cache / compilation.output

  if not p.options.parameter("--compileOnly"):
    var result = @[valgrindExecutable]
    result.add: "--error-exitcode=255"  # for zevv
    result.add: "--tool=" & (if p.an == DataRacer: "drd" else: $p.an)
    result.add: "--max-threads=50000"
    case p.an
    of Valgrind:
      result.add: ["--leak-check=full", "--show-leak-kinds=all"]
      result.add: ["--track-origins=yes"]
    of Helgrind:
      discard
    of DataRacer:
      result.add: ["--first-race-only=yes", "--join-list-vol=50000"]
      result.add: ["--report-signal-unlocked=no"]
    else:
      discard
    let userFlags = getEnv"BALLS_VALGRIND_FLAGS"
    if userFlags != "":
      result.add: userFlags
    result.add: executable
    yield result

iterator commandLine*(p: Profile; withHints = false): seq[string] =
  ## compose the interesting parts of the process invocations
  case p.an
  of anCompilerInvocation:
    for command in p.compilerCommandLine(withHints = withHints):
      yield command
  of anValgrindInvocation:
    for command in p.valgrindCommandLine(withHints = withHints):
      yield command

var pleaseCrash: Atomic[bool]
let availableProcessors = parseInt getEnv("BALLS_CORES", $countProcessors())

proc perform*(p: Profile): StatusKind =
  ## Run a single Profile `p` and return its StatusKind.
  assert not p.nonsensical
  let echoHints = getEnv"BALLS_HINTS" != ""
  let hintFree = p.commandLine(withHints = echoHints).toSeq
  let commands = p.commandLine(withHints = true).toSeq
  for index in 0..commands.high:
    if load pleaseCrash:
      result = Skip
      break
    let (output, code) = attempt commands[index]
    case code
    of 0:
      result = Pass
    else:
      result = Fail
      var chain: seq[string]
      for index, hintless in hintFree[0..<index].pairs:
        chain.add hintless.join(" ")
        chain.add "#$#/$# okay." % [ $getThreadId(), $(index + 1) ]
      chain.add hintFree[index].join(" ")
      chain.add "#$#/$# fail!" % [ $getThreadId(), $(index + 1) ]
      chain.add output
      checkpoint chain.join("\n")  # collect the complete failure history
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

proc shouldCrash(matrix: var Matrix; p: Profile): bool =
  # let the user deny crashes
  result = ballsFailFast and matrix[p] > Part and p.shouldPass
  if result:
    # before we fail the ci, run a debug test for shits and grins
    var n = p
    n.opt = debug
    if n notin matrix:      # a safer solution
      if debug in opt:      # do we even know how?
        discard perform n
        matrix[n] = Info
    let (s, code) = execCmdEx "$# --version" % [ nimExecutable ]
    if code == 0:
      checkpoint "failure; compiler $#:" % [ nimExecutable ]
      checkpoint s
    else:
      checkpoint "failure; unable to run compiler $#" % [ nimExecutable ]

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
  comeFrom monitor

proc matrixMonitor(box: Mailbox[Update]) {.cps: Continuation.} =
  ## debounce status updates received from test attempts
  var matrix: Matrix
  var mail: Update
  var last: MonoTime
  let old = if ci: 5000 else: 500
  template dirty: untyped = (getMonoTime() - last).inMilliseconds > old
  while true:
    if not box.tryRecv mail:
      # there's nothing waiting; dump the matrix?
      if dirty():
        # dump matrix updates only outside ci
        if not ci:
          checkpoint matrix
          last = getMonoTime()
      mail = recv box
    if dismissed mail:
      break
    else:
      # update the matrix with the profile->status
      tables.`[]=`(matrix, mail.profile, mail.status)
      if mail.status != Wait:
        # check to see if we should crash
        if matrix.shouldCrash(mail.profile):
          when false:
            setBallsResult int(matrix[p] > Part)
          pleaseCrash.store true
        elif not pleaseCrash.load:
          reset last
        if ci:
          # in ci, if the status is notable or we're not crashing,
          if not pleaseCrash.load or mail.status notin {Skip, Runs}:
            # show some matrix progress in case someone is watching
            checkpoint "$# $#" % [$mail.status, $mail.profile]
      # send control wherever it needs to go next
      discard trampoline(Continuation move mail)
  if dirty():
    checkpoint matrix

proc runBatch(home: Mailbox[Continuation]; monitor: Mailbox[Update];
              cache: string; profiles: seq[Profile]): StatusKind
  {.cps: Continuation.} =
  ## run a series of profiles in order
  try:
    var queue = profiles.toHeapQueue

    # mark them as waiting
    var profiles = profiles
    while profiles.len > 0:
      statusUpdate(monitor, pop(profiles), Wait)

    while queue.len > 0:
      let profile = pop queue                     # get a test to run
      result =
        if result >= Skip:                        # prior failure?
          Skip                                    # skip the remainder
        else:
          statusUpdate(monitor, profile, Runs)    # mark it running
          perform profile                         # perform the test
      statusUpdate(monitor, profile, result)      # record the status
  finally:
    removeDir cache                               # remove the cache

const MonitorService = whelp matrixMonitor
proc perform*(profiles: seq[Profile]) =
  ## concurrent testing of the provided profiles
  if profiles.len == 0:
    return  # no profiles, no problem

  # batch the profiles according to their cache
  var batches: Table[string, seq[Profile]]
  for profile in profiles.items:
    let cache = profile.cache
    if cache in batches:
      batches[cache].add profile
    else:
      batches[cache] = @[profile]

  # make a pool of workers and send them the batches
  let workers = newMailbox[Continuation]()
  let updates = newMailbox[Update]()
  var pool = newPool(ContinuationWaiter, workers, availableProcessors)

  # setup a debouncing matrix monitor
  var monitor = MonitorService.spawn(updates)
  defer: quit monitor

  for cache, profiles in batches.pairs:
    workers.send:
      whelp runBatch(workers, updates, cache, profiles)

  # shut down the runtimes as they complete the work
  for runtime in 1..pool.count:
    workers.send nil.Continuation

  # drain the pool
  while not pool.isEmpty and not pleaseCrash.load:
    drain pool
  if pleaseCrash.load:
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

  # run the profiles
  perform profiles
