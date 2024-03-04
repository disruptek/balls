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
import std/pathnorm
import std/sequtils
import std/sets
import std/streams
import std/strformat
import std/strutils
import std/tables
import std/times

import pkg/insideout
import pkg/cps
import pkg/ups/sanitize
import pkg/ups/paths
import pkg/ups/compilers
import pkg/ups/versions

import balls/spec
import balls/style
import balls/tabouli
import balls/grok/time
import balls

const
  ballsFailFast* {.booldefine.} = true ##
  ## if true (default), quit early on a test failure
  ballsUseValgrind* {.booldefine.} = true ##
  ## if true (default), attempt to use compiler sanitizers
  ballsUseSanitizers* {.booldefine.} = true ##
  ## if true (default), attempt to use valgrind
  ballsPatterns* {.strdefine.} = "glob" ##
  ## pattern matching style; "glob" (default) or "regex"
  ballsSmokeTest* {.strdefine.} = "debug"  ##
  ## default optimizations for smoke test (`balls` without options)

when ballsPatterns == "regex":
  import pkg/regex
else:
  import pkg/glob

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
    USanitizer = "usan"
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
  anValgrindInvocation* = {Valgrind, Helgrind, DataRacer}
  anSanitizerInvocation* = {ASanitizer, TSanitizer, USanitizer}
  anCompilerInvocation* = {Execution} + anSanitizerInvocation

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
  ## massage a test filename into a shorter, more legible form
  let short = extractFilename changeFileExt(fn, "")
  case short
  of "t":
    "t"  # 🙄
  elif short.startsWith("test") or short[0] != 't':
    short # okay, buddy
  else:
    short[1..short.high]

proc normalPath(filename: string): string =
  ## normalize a path to unix-style whatfer pattern matching
  result = filename.normalizePath('/')

proc shortPath*(fn: string): string =
  ## massage a test filename path into a shorter, more legible form
  let fn = normalPath fn
  let splat = splitFile fn
  if splat.dir == "tests":
    # tests/tread.nim -> read
    splat.name.short
  elif splat.dir.isRelativeTo "tests":
    # tests/api/tread.nim -> api/read
    splat.dir.tailDir / splat.name.short
  else:
    # examples\random.nim -> examples/random
    splat.dir / splat.name

proc `$`(p: Profile): string =
  "$#: $# $# $# $#" % [ p.fn.shortPath, $p.be, $p.gc, $p.opt, $p.an ]

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

proc nearby(p: Profile): auto =
  ## how we group profiles to the same row
  result = (p.fn.shortPath, ord p.be, ord p.gc)

proc parameters*(): seq[string] =
  ## ladies and gentlemen, the command-line arguments
  for i in 1..paramCount():
    if paramStr(i).startsWith "-":
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
  proc `specified tipe`*(params: seq[string]): seq[tipe] {.used.} =
    let params = map(params, toLowerAscii)
    for value in tipe:
      for prefix in prefixes.items:
        if (prefix & $value).toLowerAscii in params:
          result.add value

  proc `filtered tipe`*(params: seq[string]): seq[string] {.used.} =
    result = params
    for value in tipe:
      for prefix in prefixes:
        result =
          result.filterIt:
            it.toLowerAscii != (prefix & $value).toLowerAscii

makeSpecifier(MemModel, ["--gc:", "--mm:"])
makeSpecifier(Backend, ["-b:", "--backend:"])
makeSpecifier(Optimizer, ["-d:", "--define:"])

proc toSet[T: int8 | int16 | enum | uint8 | uint16 | char](ss: openArray[T]): set[T] =
  for item in ss.items:
    result.incl item

let ci* = getEnv("GITHUB_ACTIONS", "false") == "true"
# set some default matrix members (profiles)
var opt* = {
  debug: @["--debuginfo:on",
           "--lineTrace:on", "--stackTrace:on", "--excessiveStackTrace:on"],
  release: @["--define:release",
             "--lineTrace:on", "--stackTrace:on", "--excessiveStackTrace:on"],
  danger: @["--define:danger"]
}.toOrderedTable

# use the optimizations specified on the command-line
var specifiedOpt = specifiedOptimizer(parameters()).toSet
if specifiedOpt == {}:
  if ci:
    # no optimizations were specified; omit debug
    opt.del debug
  else:
    # smoke-test; `balls` run without optimization switches
    case try: parseEnum[Optimizer](ballsSmokeTest) except ValueError: debug
    of debug:
      opt.del release
      opt.del danger
    of release:
      opt.del debug
      opt.del danger
    of danger:
      opt.del debug
      opt.del release
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
var defaults* = @["--incremental:off", "--parallelBuild:1"]

let nimExecutable = findExe"nim"
var compilerVersion: Option[CompilerVersion]
let valgrindExecutable = findExe"valgrind"
let useValgrind = "" != valgrindExecutable and
  parseBool(getEnv("BALLS_VALGRIND", $ballsUseValgrind))
let useSanitizers = parseBool(getEnv("BALLS_SANITIZERS", $ballsUseSanitizers))

proc shortCompilerVer*(cv: CompilerVersion): string =
  result = fmt"{cv.language}-{cv.version}"
  if cv.extra != "":
    result.add "-"
    result.add cv.extra

proc shortCompilerVer*(cv: Option[CompilerVersion]): string =
  if cv.isSome:
    shortCompilerVer cv.get
  elif defined(isNimSkull):
    "nimskull-" & NimVersion
  else:
    "nim-" & NimVersion

proc longCompilerVersion*(cv: Option[CompilerVersion]; exe = nimExecutable): string =
  result = fmt"{cv.shortCompilerVer} in {exe}"
  if cv.isSome:
    result.add "\n"
    result.add fmt"date {get(cv).date} sha1 {get(cv).git}"

proc `$`*(cv: CompilerVersion): string =
  shortCompilerVer cv

proc options*(p: Profile): seq[string] =
  result = defaults & opt[p.opt]

  # add a memory management option if appropriate
  if p.gc != vm:
    if ci:
      # skip the spam when on ci
      when defined(isNimSkull):
        result.add "--gc:" & $p.gc
      else:
        result.add "--mm:" & $p.gc
    else:
      # use a switch which will likely work and merely spam
      # mainline nim users with, "hey, we now use --gc:..."
      result.add "--gc:" & $p.gc

  # grab the user's overrides provided on the command-line
  var params = parameters()

  # filter out any "specified" options we've already consumed
  params = filteredMemModel params
  params = filteredOptimizer params
  params = filteredBackend params

  when defined(isNimSkull):
    if not params.parameter("threads:off"):
      params.add "--threads:on"

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
      "--debuginfo:on",
      # Enable line directives to map C lines back to Nim
      "--linedir:on",
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
  of USanitizer:
    result.add """--passC:"-fsanitize=undefined""""
    result.add """--passL:"-fsanitize=undefined""""
    result.add """--passC:"-fno-sanitize-recover=undefined""""
    result.add """--passL:"-fno-sanitize-recover=undefined""""
  else:
    discard

proc nonsensical*(an: Analyzer): bool =
  ## certain analyzers need not be attempted
  if an in anValgrindInvocation and not useValgrind:
    true
  elif an in anSanitizerInvocation and not useSanitizers and not ci:
    true
  elif an != Execution and danger notin opt:
    true
  else:
    false

proc nonsensical*(p: Profile): bool =
  ## certain profiles need not be attempted
  if p.gc == vm and p.be notin {js, e}:
    true
  elif p.be in {js, e} and p.gc != vm:
    true
  elif p.fn == changeFileExt(p.fn, "nims") and p.gc != vm:
    true
  elif p.an.nonsensical:
    true
  elif p.an != Execution and p.opt notin {danger}:
    true
  elif p.an != Execution and p.gc notin {arc, orc}:
    true
  elif p.an != Execution and p.options.parameter("--compileOnly"):
    true
  else:
    false

iterator rowPermutations(matrix: Matrix; p: Profile): Profile =
  ## emit the profile permutations appropriate for this row
  var p = p
  for an in Analyzer.items:
    case an
    of Execution:
      for optimizer in Optimizer.items:
        if optimizer in opt:
          p.an = an
          p.opt = optimizer
          yield p
    elif not an.nonsensical:
      p.an = an
      p.opt = danger
      yield p

proc matrixTable*(matrix: Matrix): string =
  ## Render the `matrix` as a table.
  var matrix = matrix
  var tab = Tabouli()
  tab.headers = @[shortCompilerVer(compilerVersion), "", ""]
  tab.freeze = len tab.headers
  for an in Analyzer.items:
    case an
    of Execution:
      for optimizer in Optimizer.items:
        if optimizer in opt:
          tab.headers.add $optimizer
    elif not an.nonsensical:
      tab.headers.add $an

  # while the matrix has members,
  while matrix.len > 0:
    # reorder the remaining profiles by their display order
    let profiles = toSeq(matrix.keys).sortedByIt(it.nearby)

    # the first profile in that list is the best one to show next
    var p = profiles[0]

    # compose a row's prefix labels
    var row = @[p.fn.shortPath, $p.be, $p.gc]
    assert row.len == tab.freeze, "counting is hard"

    # then iterate over the memory models and consume any results
    for p in rowPermutations(matrix, p):
      # pull the run out of the matrix if possible
      # (we can't use pop|take whatfer nim-1.0 reasons)
      if p in matrix:
        let status = matrix[p]
        row.add:
          if style.useColor:
            $statusStyles[status] & $status
          else:
            $status
      else:
        row.add " "
      matrix.del p        # we have to scrub all matching profiles thusly
    # only add rows with some kind of status entry
    if row[tab.freeze..^1].join(" ").strip() != "":
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
  if ci:
    if p.opt notin {danger}:
      # ignore performance warnings outside of local danger builds
      omit.add "Performance"
    when defined(isNimSkull):
      omit.add "LineTooLong"
  for hint in omit.items:
    result.add "--hint[$#]=off" % [ hint ]

  ## compute --warning(s) as appropriate
  omit = @[]
  if ci:
    # remove spam from ci logs
    omit.add ["UnusedImport", "ProveInit", "ObservableStores",
              "UnreachableCode"]
    when not defined(isNimSkull):
      omit.add ["CaseTransition", "BareExcept"]
  for warning in omit.items:
    result.add "--warning[$#]=off" % [ warning ]

proc cache*(p: Profile): string =
  ## come up with a unique cache directory according to where you'd like
  ## to thread your compilations under ci or local environments.

  # FIXME: this gets replaced with a config sniff?

  let opt =
    case p.an
    of ASanitizer, TSanitizer, USanitizer:
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

proc pleaseExit(): bool =
  ## decide whether to exit depending upon ballsFailFast
  ## and whether any test which shouldPass() has failed
  ballsFailFast and pleaseCrash.load

let availableProcessors = parseInt getEnv("BALLS_CORES", $countProcessors())

proc perform*(p: Profile): StatusKind =
  ## Run a single Profile `p` and return its StatusKind.
  assert not p.nonsensical
  let echoHints = getEnv"BALLS_HINTS" != ""
  let hintFree = toSeq commandLine(p, withHints = echoHints)
  let commands = toSeq commandLine(p, withHints = true)
  for index in 0..commands.high:
    if pleaseExit():
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
  result = matrix[p] > Part and p.shouldPass
  if result:
    # before we fail the ci, run a debug test for shits and grins
    var n = p
    n.opt = debug
    if n notin matrix:      # a safer solution
      if debug in opt:      # do we even know how?
        discard perform n
        matrix[n] = Info
    if compilerVersion.isSome:
      checkpoint "failure; compiler $#" %
        [ longCompilerVersion(compilerVersion, nimExecutable) ]
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
  var began: Table[Profile, MonoTime]
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
      case mail.status
      of Wait: discard
      of Runs:
        began[mail.profile] = getMonoTime()  # remember when we started
      else:
        # check to see if we should crash
        if matrix.shouldCrash(mail.profile):
          when false:
            setBallsResult int(matrix[p] > Part)
          pleaseCrash.store true
        elif not pleaseExit():
          reset last
      if ci:
        # in ci, if the status is notable or we're not crashing,
        if not pleaseExit() and mail.status notin {Skip, Wait}:
          # show some matrix progress in case someone is watching
          if mail.status > Runs and mail.profile in began:
            let took = shortDuration: getMonoTime() - began[mail.profile]
            checkpoint fmt"{mail.status} {mail.profile:<66} {took:>7}"
          else:
            checkpoint fmt"{mail.status} {mail.profile:<66}"
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
  var batches: OrderedTable[string, seq[Profile]]
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
  while not pool.isEmpty and not pleaseExit():
    drain pool
  if pleaseCrash.load:
    quit 1

proc profiles*(fn: string): seq[Profile] =
  ## Produce profiles for a given test filename.
  var profile: Profile
  profile.fn = fn
  # NOTE:
  # if we're testing more than one backend, it's probably
  # because we aren't expecting any to fail; run through
  # them one-at-a-time since it's most likely that our
  # tests fail, for whatever reason, on the first backend
  for backend in Backend.items:
    if backend in be:
      profile.be = backend
      # NOTE: probe for leaks in orc before we check for races in arc
      for analyzer in Analyzer.items:
        profile.an = analyzer
        # NOTE: sanitizers are essentially optimizers
        for optimizer in Optimizer.items:
          if optimizer in opt:
            profile.opt = optimizer
            for memory in MemModel.items:
              if memory in gc:
                profile.gc = memory
                if not profile.nonsensical:
                  result.add profile

when ballsPatterns == "regex":
  const directoryPattern = "(/[^/]+)*/.*"
  const testDirPattern = "(/[^/]+)*/t.*"
  type Pattern = Regex2
  proc makePattern*(pattern: string): Pattern =
    ## Compile a regex pattern.
    Pattern: re2(pattern & "\\.nim$")
  proc doesMatch*(filename: string; pattern: Pattern): bool =
    ## Determine if a filename is unmasked by a regex.
    pattern in filename.normalPath
else:
  const directoryPattern = "/***"
  const testDirPattern = "/**/t*"
  type Pattern = Glob
  proc makePattern*(patt: string): Pattern =
    ## Compile a glob pattern.
    Pattern: glob(patt & ".nim")
  proc doesMatch*(filename: string; patt: Pattern): bool =
    ## Determine if a filename is unmasked by a glob.
    filename.normalPath.matches patt
const testPattern* = "tests" & testDirPattern

proc ordered*(directory: string; pattern: Pattern): seq[string] =
  ## Order a `directory` tree of test files recursively,
  ## selecting tests according to the `pattern`.
  if not directory.dirExists: return @[]
  # collect the filenames recursively
  for file in walkDirRec(directory, yieldFilter = {pcFile, pcLinkToFile}):
    if file.doesMatch(pattern):
      result.add(file)
  when true:
    # sort them by name, alphabetically
    result.sort(system.cmp, Ascending)
  else:
    # sort them by age, recently-changed first
    proc age(path: string): Time =
      getFileInfo(path, followSymlink = true).lastWriteTime
    proc byAge(a, b: string): int = system.cmp(a.age, b.age)
    result.sort(byAge, Descending)

proc findDefaultTests*(): seq[string] =
  ## Find the default tests from the current directory; tests, or all source files.
  var pattern: Pattern
  pattern = makePattern testPattern
  result = ordered("tests", pattern)
  try:
    # if we've found no tests so far,
    let patt = "." & directoryPattern
    if result.len == 0:
      # try to find tests from the current directory
      result = ordered(".", makePattern patt)
  except OSError as e:
    checkpoint "bad news about the current directory... it's gone?"
    checkpoint "the os says `$#`" % [ e.msg ]
    quit 1

proc findTestsViaPatterns*(patts: openArray[string]): seq[string] =
  ## Find tests from the current directory which match patterns.
  var pattern: Pattern
  for patt in patts.items:
    if patt.dirExists:
      # the pattern is a directory; assume it's NOT tests-like
      pattern = makePattern(patt & directoryPattern)
      result &= ordered(patt, pattern)
    elif patt.fileExists:
      # the pattern is a file; assume it's a test
      result &= @[patt]
    else:
      # perform a pattern search from the current directory
      pattern = makePattern patt
      result &= ordered(".", pattern)

proc main*(patts: openArray[string]) =
  ## Run each of `pattern`-matching tests, if provided, else run the default tests.
  var tests: seq[string]
  if 0 == patts.len:
    tests = findDefaultTests()
    if tests.len == 0:
      checkpoint "no tests found; that's good, right?"
      quit 0
  else:
    tests = findTestsViaPatterns(patts)
    if tests.len == 0:
      checkpoint "no tests found for patterns:", repr(patts)
      quit 0

  # see if we can figure out which compiler this is
  compilerVersion = runCompilerVersion()
  if compilerVersion.isNone:
    checkpoint "unable to find/parse compiler --version; continuing..."

  # generate profiles for the ordered inputs
  var profiles: seq[Profile]
  for test in tests.items:
    profiles &= test.profiles

  # run the profiles
  perform profiles
