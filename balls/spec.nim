import std/times
import std/macros
import std/os

import grok

when defined(danger):
  import grok/mem # for quiesceMemory()
  # zero-cost cause the symbol to be ".used."
  assert quiesceMemory is proc(message: string): int

# critically, if we ever indicated a failure,
# don't obscure that failure with a subsequent success

when defined(js):
  # import the process.exitCode from javascript
  var exitCode {.importjs: "process.$1".}: cint
  # we may as well also import the process.exit()
  proc processExit(code: cint = 0) {.importjs: "process.$1(#)".}
  proc setBallsResult(q: int) =
    # set the process.exitCode
    exitCode = max(exitCode, cint q)
  # this will be available only in javascript
  export processExit
elif defined(nimscript):
  # nimscript's implementation
  proc setBallsResult(q: int) =
    programResult = max(programResult, q)
  proc addExitProc(p: proc() {.noconv.}) = addQuitProc p
  export addExitProc
else:
  when (NimMajor, NimMinor) >= (1, 3):
    # this is our ideal
    import std/exitprocs
    proc setBallsResult(q: int) =
      setProgramResult:
        max(getProgramResult(), q)
  else:
    proc setBallsResult(q: int) =
      programResult = max(programResult, q)
    proc addExitProc(p: proc() {.noconv.}) = addQuitProc p
  # this will be available only inside the c/cpp backends
  export addExitProc
# make sure we definitely have this symbol on all backends
export setBallsResult

const hasPanics* =
  # panics were introduced in 1.1.1 ...
  # when (NimMajor, NimMinor, NimPatch) >= (1, 1, 1):
  # ... but we need to support --useversion:1.0
  defined(nimPanics)

# working around the Error->Defect change-over
when (NimMajor, NimMinor) >= (1, 3):
  const hasDefects* = true
else:
  # this is the best solution to --useVersion:1.0 i guess...
  const hasDefects* = compiles(AssertionDefect)
  when not hasDefects:
    type AssertionDefect* = AssertionError

const
  ballsDry* {.booldefine.} = false
  onCI* {.used.} = getEnv("GITHUB_ACTIONS", "false") == "true"

  testable* = {
    # these are safe to wrap individually inside a try/except block
    nnkBlockStmt, nnkIfStmt, nnkWhileStmt, nnkForStmt, nnkTryStmt,
    nnkReturnStmt, nnkYieldStmt, nnkDiscardStmt, nnkContinueStmt,
    nnkAsmStmt, nnkImportStmt, nnkImportExceptStmt, nnkExportStmt,
    nnkExportExceptStmt, nnkFromStmt, nnkIncludeStmt, nnkCommand,
    nnkCall, nnkWhenStmt
  }

when ballsDry:
  const
    emojiStack*  = " ^ "
    emojiSource* = " > "
  type
    StatusKind* = enum      ## possible test results
      None = " "      ## (undefined)
      Info = "#"      ## may prefix information
      Wait = "."      ## pending cache access
      Runs = "?"      ## currently runs
      Pass = "+"      ## total success
      Skip = "$"      ## test was skipped
      Part = "/"      ## partial success
      Fail = "-"      ## assertion failure
      Died = "*"      ## unexpected exception
      Oops = "!"      ## compiles() failed

else:
  const
    emojiStack*  = " 🗇 "
    emojiSource* = " 🗏 "
  type
    StatusKind* = enum      ## possible test results
      None = "  "          ## (undefined)
      Info = "🔵"          ## may prefix information
      Wait = "⏳"          ## pending cache access
      Runs = "🏃"          ## currently runs
      Pass = "🟢"          ## total success
      Skip = "❔"          ## test was skipped
      Part = "🟡"          ## partial success
      Fail = "🔴"          ## assertion failure
      Died = "💥"          ## unexpected exception
      Oops = "⛔"          ## compiles() failed

type
  FailError* = object of CatchableError
  SkipError* = object of CatchableError
  ExpectedError* = object of CatchableError

  Test* = object
    status*: StatusKind    ## the result of the test
    code*: NimNode         ## the user's original code
    node*: NimNode         ## the test and its instrumentation
    name*: string          ## the name of the test, duh
    number*: int           ## tests tend to get unique numbers
    clock*: float          ## used to measure test timing
    memory*: int           ## used to measure test memory

var testCount {.compileTime.}: int                       # whatfer counting!

proc totalTests*(): int =
  ## reveal the value of the counter without exposing it
  testCount

proc init*(test: var Test; name: string; code: NimNode) =
  ## initialize a test with the most basic input possible
  test.name = name
  test.code = code
  test.node = nnkStmtList.newNimNode(code)

  inc testCount
  test.number = testCount

  # we've stored the original code in the Test object, so now we
  # copy the input and put it into a new statement list; `node` will
  # hold the code that we'll actually run to instrument the test
  test.node = newStmtList copyNimTree(code)

  if code.kind notin testable:
    # it's not testable; indicate that it worked (what else?)
    when defined(release):
      test.status = Pass

    # if the test result is pre-ordained due to code features, revert
    # the test number so as not to make the user think we skipped some
    # tests
    test.number = 0        # set the test number to zero as a sentinel
    dec testCount

proc dollar*(n: NimNode): NimNode =
  ## If it's not a string literal, dollar it.
  if n.kind == nnkStrLit:
    return n
  if n.kind == nnkCall:
    if n.len > 0:
      if n[0].kind in {nnkIdent, nnkSym}:
        if n[0].strVal in ["$", "&"]:
          return n
  result = nnkCall.newTreeFrom n:
    bindSym"$"
    n

proc flushStreams*() {.noconv, used.} =
  ## Convenience for flushing `stdmsg()` during process exit.
  when not defined(js) and not defined(nimscript):
    flushFile stdmsg()

when compileOption"threads":
  import std/rlocks
  var clobber {.global.}: RLock
  initRLock clobber

  template noclobber*(body: untyped) =
    ## serialize access to the body; usually for output reasons
    withRLock clobber:
      body
else:
  template noclobber*(body: untyped) = body
