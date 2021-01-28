import std/times
import std/macros
import std/os

import grok/mem  # for quiesceMemory()

when (NimMajor, NimMinor) >= (1, 3):
  import std/exitprocs
  const hasDefects* = true
  export setProgramResult, addExitProc
else:
  # this is the best solution to --useVersion:1.0 i guess...
  const hasDefects* = compiles(AssertionDefect)
  when not hasDefects:
    type AssertionDefect* = AssertionError
  proc setProgramResult*(q: int) =
    programResult = q
  proc addExitProc*(p: proc() {.noconv.}) = addQuitProc p

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
      Info = "i"      ## may prefix information
      Pass = "🗸"      ## total success
      Skip = "?"      ## test was skipped
      Part = "/"      ## partial success
      Fail = "𐄂"      ## assertion failure
      Died = "✷"      ## unexpected exception
      Oops = "؟"      ## compiles() failed

else:
  const
    emojiStack*  = " 🗇 "
    emojiSource* = " 🗏 "
  type
    StatusKind* = enum      ## possible test results
      None = "  "          ## (undefined)
      Info = "🔵"          ## may prefix information
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
    result = n
  elif n.kind == nnkCall and $n[0] in ["$", "&"]:
    result = n
  else:
    result = newCall(bindSym"$", n)

proc flushStderr*() {.noconv, used.} =
  ## Convenience for flushing stderr during process exit.
  flushFile stderr
