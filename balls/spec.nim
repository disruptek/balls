import std/macrocache
import std/macros
import std/os
import std/times

import grok

const
  ballsAuditTimeSpace* {.booldefine.} =
    defined(danger) and not defined(nimscript)

when ballsAuditTimeSpace:
  import grok/mem
  export quiesceMemory

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
    emojiStack*  = "^"
    emojiSource* = ">"
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
    emojiStack*  = "🗇"
    emojiSource* = "🗏"
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

const testCount = CacheCounter"balls.spec.testCount"   # whatfer counting!

proc totalTests*(): int =
  ## reveal the value of the counter without exposing it
  testCount.value

proc init*(test: var Test; name: string; code: NimNode) =
  ## initialize a test with the most basic input possible
  test.name = name
  test.code = code
  test.node = nnkStmtList.newNimNode(code)

  inc testCount
  test.number = testCount.value

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
    inc testCount, -1

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

proc localPath*(fn: string): string =
  ## a somewhat verbose impl due to necessity
  when nimvm:
    when (NimMajor, NimMinor) == (1, 4):
      result = fn
    else:
      result = relativePath(fn, getProjectPath())
  else:
    when defined(js):
      block done:
        when (NimMajor, NimMinor) >= (1, 4):
          try:
            result = relativePath(fn, getCurrentDir())
            break done
          except ValueError:
            # "specified root is not absolute"; cwd probably unavailable
            discard
        result = relativePath(fn, getProjectPath())
    else:
      result = relativePath(fn, getCurrentDir())
