import std/algorithm
import std/times
import std/os
import std/sequtils
import std/terminal
import std/strutils
import std/macros
import std/colors
import std/streams

when (NimMajor, NimMinor) >= (1, 3):
  import std/exitprocs
  const hasDefects = true
else:
  # this is the best solution to --useVersion:1.0 i guess...
  const hasDefects = compiles(AssertionDefect)
  when not hasDefects:
    type AssertionDefect = AssertionError
  proc setProgramResult(q: int) =
    programResult = q

import grok/mem
import grok/time

when defined(release):
  # we only need this for release tests
  import bytes2human

when defined(windows):
  export execShellCmd

const
  statements {.used.} = {
    # these are not r-values

    nnkBlockStmt, nnkStmtList, nnkIfStmt, nnkWhileStmt, nnkVarSection,
    nnkLetSection, nnkConstSection, nnkWhenStmt, nnkForStmt, nnkTryStmt,
    nnkReturnStmt, nnkYieldStmt, nnkDiscardStmt, nnkContinueStmt,
    nnkBreakStmt, nnkAsmStmt, nnkImportStmt, nnkImportExceptStmt,
    nnkExportStmt, nnkExportExceptStmt, nnkFromStmt, nnkIncludeStmt,
    nnkTypeSection, nnkMixinStmt, nnkBindStmt, nnkProcDef, nnkIteratorDef,
    nnkConverterDef, nnkTemplateDef, nnkFuncDef, nnkMacroDef, nnkCommand,
    nnkCall

  }

  testable = {
    # these are safe to wrap individually inside a try/except block

    nnkBlockStmt, nnkIfStmt, nnkWhileStmt, nnkForStmt, nnkTryStmt,
    nnkReturnStmt, nnkYieldStmt, nnkDiscardStmt, nnkContinueStmt,
    nnkAsmStmt, nnkImportStmt, nnkImportExceptStmt, nnkExportStmt,
    nnkExportExceptStmt, nnkFromStmt, nnkIncludeStmt, nnkCommand,
    nnkCall, nnkWhenStmt

  }

type
  FailError = object of CatchableError
  SkipError = object of CatchableError
  StatusKind = enum
    None = "  "
    Info = "🔵"          ## may prefix information
    Okay = "🟢"          ## total success
    Skip = "❔"          ## test was skipped
    Part = "🟡"          ## partial success
    Fail = "🔴"          ## assertion failure
    Died = "💥"          ## unexpected exception
    Oops = "⛔"          ## compiles() failed

  Test = object
    status: StatusKind    ## the result of the test
    orig: NimNode         ## the user's original code
    n: NimNode            ## the test and its instrumentation
    name: string          ## the name of the test, duh
    number: int           ## tests tend to get unique numbers
    clock: float          ## used to measure test timing
    memory: int           ## used to measure test memory

  Styling = distinct string

  Rewrite = proc(n: NimNode): NimNode

when false:
  var tests: seq[Test]      ## all the tests
var clock: float          ## pre-test time
var memory: int           ## pre-test memory

proc rewrite(n: NimNode; r: Rewrite): NimNode =
  ## perform a recursive rewrite (at least once) using the given mutator
  result = r(n)
  if result.isNil:
    result = copyNimNode n
    for kid in items(n):
      result.add rewrite(kid, r)
    let second = r(result)
    if not second.isNil:
      result = second

proc `&`(a, b: Styling): Styling {.borrow.}
proc `&`(a: Styling; b: string): Styling = a & Styling(b)
proc `&`(a: string; b: Styling): Styling = b & a

{.push hint[ConvFromXtoItselfNotNeeded]: off.}

const
  resetStyle      = Styling ansiResetCode
  resultsStyle    = Styling ansiStyleCode(styleItalic) &
                    Styling ansiForegroundColorCode(fgWhite, true)
  commentStyle    = Styling ansiStyleCode(styleItalic) &
                    Styling ansiForegroundColorCode(fgWhite, true)
  lineNumStyle    = Styling ansiStyleCode(styleItalic) &
                    Styling ansiForegroundColorCode(fgBlack, true)
  successStyle    = Styling ansiForegroundColorCode(fgGreen)
  oopsStyle       = Styling ansiStyleCode(styleBright) &
                    Styling ansiStyleCode(styleReverse) &
                    Styling ansiBackgroundColorCode(Color(0xFFFFFF)) &
                    Styling ansiForegroundColorCode(fgRed, true)
  failureStyle    = Styling ansiForegroundColorCode(fgRed)
  skippedStyle    = Styling ansiStyleCode(styleStrikethrough) &
                    Styling ansiForegroundColorCode(fgMagenta, false)
  exceptionStyle  = Styling ansiForegroundColorCode(fgRed, true)
  sourceStyle     = Styling ansiForegroundColorCode(fgDefault)
  viaProcStyle    = Styling ansiStyleCode(styleItalic) &
                    Styling ansiForegroundColorCode(fgBlue, false)
  viaFileStyle    = Styling ansiStyleCode(styleItalic) &
                    Styling ansiStyleCode(styleUnderscore) &
                    Styling ansiForegroundColorCode(fgBlue, true)
when defined(release): # avoid unused warnings
  const
    testNumStyle  = Styling ansiStyleCode(styleItalic) &
                    Styling ansiForegroundColorCode(fgYellow, true)

proc `$`(style: Styling): string =
  when nimvm:
    # don't try to mess with styling at compile-time
    result = style.string
  else:
    # at runtime, try to emit style if possible
    if stderr.isAtty:
      result = style.string
      if result != resetStyle.string:
        result = resetStyle.string & result

proc dollar(n: NimNode): NimNode =
  ## If it's not a string literal, dollar it.
  if n.kind == nnkStrLit:
    result = n
  elif n.kind == nnkCall and $n[0] in ["$", "&"]:
    result = n
  else:
    result = newCall(ident"$", n)

proc combineLiterals(n: NimNode): NimNode =
  ## merges "foo" & "bar" into "foobar"
  proc combiner(n: NimNode): NimNode =
    case n.kind
    of nnkCall:
      case $n[0]
      of "$":
        if n[1].kind == nnkStrLit:
          result = n[1]
      of "&":
        if len(n) == 3 and {n[1].kind, n[2].kind} == {nnkStrLit}:
          result = newLit(n[1].strVal & n[2].strVal)
      else:
        discard
    else:
      discard
  result = rewrite(n, combiner)

proc `&`(style: Styling; n: NimNode): NimNode =
  ## combine style and something $able, but only output the
  ## style if you find that the program is on a tty at runtime
  let isAtty = bindSym"isAtty"
  var n = dollar(n)
  let text = newStmtList(newLit($style), n, newLit($resetStyle))
  result = nnkIfStmt.newNimNode(n)
  result.add nnkElifBranch.newTree(newCall(isAtty, ident"stderr"),
                                   nestList(ident"&", text))
  result.add nnkElse.newTree(n)

proc comment(n: NimNode): NimNode =
  ## render a comment with the given stringish node
  result = infix(lineNumStyle & newLit"## ", "&", n)

var testCount {.compileTime.}: int                       ## whatfer counting!
var testResults = newSeq[int](1 + ord(high StatusKind))  ## result totals

proc incResults(test: Test): NimNode =
  newCall ident"inc":
    nnkBracketExpr.newTree(bindSym"testResults", newLit test.status.ord)

proc checkOne(condition: NimNode; message: NimNode): NimNode =
  ## generate a simple check statement with optional exception message
  let assertion =
    when hasDefects:
      ident"AssertionDefect"
    else:
      ident"AssertionError"
  var message =
    if message.kind == nnkStrLit and message.strVal == "":
      newLit condition.repr
    else:
      message
  var clause = nnkRaiseStmt.newTree:
    newCall(ident"newException", assertion, message)
  result = newIfStmt (newCall(ident"not", condition), clause)

macro check*(body: untyped; message = "") =
  ## Check one or more expressions (in a block); raises an AssertionDefect
  ## in the event that the expression is `false` regardless of `assertions`
  ## settings.  Optionally specify a custom message a la `assert`.
  if body.kind == nnkStmtList:
    result = newStmtList()
    for child in body.items:
      result.add: checkOne(child, message)
  else:
    result = checkOne(body, message)

proc `status=`(t: var Test; s: StatusKind) {.used.} =
  system.`=`(t.status, max(t.status, s))

proc prefixLines(s: string; p: string): string =
  ## prefix each line of multiline input with the given string
  for line in items(splitLines(s, keepEol = true)):
    result.add p & line

proc prefixLines(s: NimNode; p: string): NimNode =
  ## prefix each line of multiline input with the given string
  result = newStmtList()
  var ss: NimNode
  case s.kind
  of nnkCommentStmt:
    ss = newStmtList()
    for line in items(splitLines(s.strVal, keepEol = true)):
      ss.add line.newLit
  else:
    ss = s
  for line in items(ss):
    result.add infix(p.newLit, "&", line)
  result = nestList(ident"&", result)

proc numberLines(s: string; first = 1): NimNode =
  ## prefix each line of multiline input with a rendered line number
  result = newStmtList()
  for n, line in pairs(splitLines(s, keepEol = true)):
    var ln = lineNumStyle & align($(n + first), 3).newLit
    ln = infix(ln, "&", "  ".newLit)
    ln = infix(ln, "&", sourceStyle & line.newLit)
    result.add ln

proc checkpoint*(ss: varargs[string, `$`]) =
  ## Like `echo`, but outputs to `stderr` with the other test output.
  writeLine(stderr, ss.join " ")

proc output(n: NimNode): NimNode =
  assert not n.isNil
  let checkpoint = bindSym"checkpoint"
  result = newCall(checkpoint, combineLiterals(n))

proc output(t: Test; n: NimNode): NimNode =
  assert not n.isNil
  let prefixer = bindSym"prefixLines"
  result = output newCall(prefixer, dollar(n), newLit($t.status & " "))

proc output(test: Test; styling: Styling; n: NimNode): NimNode {.used.} =
  assert not n.isNil
  let prefixer = bindSym"prefixLines"
  result = newCall(prefixer, dollar(n), newLit($test.status & " "))
  result = test.output(styling & result)

proc report(n: NimNode): NimNode =
  ## render a multi-line comment
  let prefix = $lineNumStyle & "## " & $commentStyle
  result = output(infix(prefixLines(n, prefix),
                  "&", newLit(resetStyle.string)))

macro report*(ss: varargs[typed]) =
  ## Like `checkpoint`, but rendered as a comment.
  ## You can supply AST here to be typed and rendered via `treeRepr`.
  var s: string
  for index, value in ss.pairs:
    if value.kind == nnkStrLit:
      s.add value.strVal & " "
    else:
      s.add "\n" & value.treeRepr
  result =
    if s == "":
      newEmptyNode()
    else:
      report newCommentStmtNode(s)

proc success(t: var Test): NimNode =
  ## what to do when a test is successful
  t.status = Okay
  result = newStmtList()
  result.add t.incResults
  result.add t.output(successStyle & newLit(t.name))

proc fromFileGetLine(file: string; line: int): string =
  let lines = toSeq lines(file)
  result = lines[line - 1]

proc findWhere(s: string; p: string; into: var string): bool {.used.} =
  ## find the location of a substring and, if found, produce empty prefix
  result = s.count(p) == 1
  if result:
    into = spaces(s.find(p))

proc renderStack(prefix: string; stack: seq[StackTraceEntry]) =
  ## stylishly render a stack trace
  var path = getCurrentDir()
  var cf: string
  var result: seq[string]
  for s in items(stack):
    if cf != $s.filename:
      cf = $s.filename
      result.add "$1$3$2$1" % [ $resetStyle,
        relativePath(cf, path), $viaFileStyle ]
    let code = fromFileGetLine(cf, s.line)
    let line = align($s.line, 5)
    result.add "$1$5$2 $6$3  $7# $4()$1" % [ $resetStyle,
      line, code, $s.procname, $lineNumStyle, $sourceStyle, $viaProcStyle ]
  checkpoint result.join("\n").prefixLines prefix & " 🗇 "

proc renderTrace(t: Test; n: NimNode = nil): NimNode =
  ## output the stack trace of a test, and perhaps that of any exception
  var renderStack = bindSym"renderStack"
  var getStack = newCall(ident"getStackTraceEntries")
  if not n.isNil:
    getStack.add n  # get the exception's stacktrace
  result = newIfStmt((newCall(ident"stackTraceAvailable"),
                      newCall(renderStack, newLit($t.status), getStack)))

proc renderSource(t: Test): NimNode =
  ## strip the first comment, include the remainder
  var node = copyNimTree(t.orig)
  var info = lineInfoObj(t.orig)
  if len(node) > 0:
    if node[0].kind == nnkCommentStmt:
      let dropFirst = node[0].strVal.splitLines(keepEol = true)[1..^1].join("")
      node[0] = newCommentStmtNode(dropFirst)
  result = t.output(repr(node).numberLines(info.line).prefixLines(" 🗏 "))

proc setExitCode(t: Test; code = QuitFailure): NimNode =
  let isAtty = bindSym"isAtty"
  let setResult = bindSym"setProgramResult"
  result = newIfStmt((prefix(newCall(isAtty, ident"stderr"), "not"),
                      newCall(setResult, code.newLit)))

proc failure(t: var Test; n: NimNode = nil): NimNode {.used.} =
  ## what to do when a test fails
  t.status = Fail
  result = newStmtList()
  result.add t.incResults
  result.add t.output(failureStyle & newLit(t.name))
  result.add t.renderSource
  result.add t.renderTrace(n)
  result.add t.setExitCode

proc dotMsg(n: NimNode): NimNode =
  ## render an exception's .msg field
  result = commentStyle & newDotExpr(n, ident"msg")

proc exceptionString(n: NimNode): NimNode =
  ## render an exception with name and message
  assert not n.isNil
  result = infix(infix(dollar(newDotExpr(n, ident"name")),
                       "&", ": ".newLit), "&", n.dotMsg)

proc badassert(t: var Test; n: NimNode = nil): NimNode =
  ## like failure(), but don't render the stack trace
  t.status = Fail
  result = newStmtList()
  result.add t.incResults
  if n.isNil:
    result.add t.output(failureStyle & newLit(t.name))
  else:
    let text = newStmtList(t.name.newLit, newLit(": "), n.dotMsg)
    result.add t.output(failureStyle & nestList(ident"&", text))
  result.add t.renderSource
  result.add t.setExitCode

proc skipped(t: var Test; n: NimNode): NimNode =
  ## what to do when a test is skipped
  assert not n.isNil
  t.status = Skip
  result = newStmtList()
  result.add t.incResults
  var text = newStmtList()
  text.add(skippedStyle & t.name.newLit)
  text.add(": ".newLit)
  text.add(n.dotMsg)
  result.add t.output(nestList(ident"&", text))

proc exception(t: var Test; n: NimNode): NimNode =
  ## what to do when a test raises an exception
  assert not n.isNil
  t.status = Died
  let text = newStmtList(newLit(t.name & ": "), n.exceptionString)
  result = newStmtList()
  result.add t.incResults
  result.add t.output(exceptionStyle & nestList(ident"&", text))
  result.add t.renderSource
  result.add t.renderTrace(n)
  result.add t.setExitCode

proc reportResults(): NimNode =
  ## produce a small legend showing result totals
  var checkpoint = bindSym"checkpoint"
  var results = bindSym"testResults"
  var legend = newStmtList()
  legend.add comment(resultsStyle & newLit($testCount & " tests  "))
  for status in items(StatusKind):
    legend.add newLit"  "
    let brack = nnkBracketExpr.newTree(results, newLit status.ord)
    legend.add newTree(nnkIfStmt,
               newTree(nnkElIfBranch, infix(brack, ">", 0.newLit),
                       infix(newLit $status, "&", dollar(brack))),
               newTree(nnkElse, newLit""))
  result = newCall(checkpoint, combineLiterals(nestList(ident"&", legend)))

proc composeColon(name: NimNode;
                  value: int | enum | float | string | NimNode): NimNode =
  when value is int:
    result = newColonExpr(name, newLit value)
  elif value is StatusKind:
    let status = bindSym"StatusKind"
    result = newColonExpr(name, newCall(status, newLit ord(value)))
  elif value is float:
    result = newColonExpr(name, newLit value)
  elif value is string:
    result = newColonExpr(name, newLit value)
  elif value is ref:
    result = newColonExpr(name, newNilLit())
  else:
    result = newEmptyNode()

proc ctor(test: Test): NimNode =
  ## copy compile-time Test into, uh, compile-time Test constructor
  let typ = bindSym"Test"
  result = nnkObjConstr.newTree(typ)
  for name, value in fieldPairs(test):
    when value isnot ref:
      result.add composeColon(ident(name), value)

when defined(release):
  proc pad(n: NimNode; size: int): NimNode =
    let align = bindSym"align"
    result = newCall(align, newCall(ident"$", n), size.newLit)

  proc humanize(n: NimNode): NimNode =
    ## convert bytes to human-readable form
    template abs(n: NimNode): NimNode = newCall(bindSym"abs", n)
    let human = bindSym"bytes2human"
    result = nnkIfExpr.newTree
    result.add:
      nnkElifBranch.newTree(
        infix(n, ">", 0.newLit),
        infix(newLit"+", "&", newDotExpr(newCall(human, n), ident"short")))
    result.add:
      nnkElifBranch.newTree(infix(n, "==", 0.newLit), newLit"")
    result.add:
      nnkElse.newTree infix(newLit"-", "&",
                            newDotExpr(newCall(human, abs n), ident"short"))

proc postTest(test: Test): NimNode =
  ## run this after a test has completed
  result = newStmtList()
  let temp = genSym(nskVar, "test")
  result.add newVarStmt(temp, ctor(test))

  when defined(release):
    let tempClock = newDotExpr(temp, ident"clock")
    let tempMem = newDotExpr(temp, ident"memory")

    # record the duration
    result.add newAssignment(tempClock, infix(newCall(bindSym"epochTime"),
                                              "-", bindSym"clock"))
    # record the memory
    let quiesce = newCall(bindSym"quiesceMemory", newLit"")
    result.add newAssignment(tempMem, infix(quiesce, "-", bindSym"memory"))

    let newDur = bindSym"initDuration"
    let nano = newCall(newDur,
                       newTree(nnkExprEqExpr, ident"nanoseconds",
                               newCall(ident"int",    # convert it to int
                                infix(tempClock, "*", # nano/sec
                                      1_000_000_000.newLit))))

    # compose the status line
    var text = newStmtList()
    text.add testNumStyle & pad(newLit($test.number), 5)
    # the change in memory footprint after the test
    text.add pad(humanize tempMem, 30)
    # the short duration representing how long the test took
    text.add pad(newCall(bindSym"shortDuration", nano), 20)
    result.add output(comment(nestList(ident"&", text)))

  # stash it in the sequence?
  when false:
    let tests = bindSym"tests"
    result.add newCall(ident"add", tests, temp)


proc compilerr(t: var Test): NimNode {.used.} =
  ## the compiler wasn't able to compile the test
  t.status = Oops
  result = newStmtList()
  result.add t.incResults
  result.add t.output(oopsStyle & newLit(t.name & ": compile failed"))
  result.add t.renderSource
  result.add t.setExitCode
  result.add t.postTest

proc skip*(msg = "skipped") =
  ## Manually skips the remainder of the current test.
  raise newException(SkipError, msg)

proc fail*(msg = "failed") =
  ## Manually fails the current test.
  raise newException(FailError, msg)

proc wrapExcept(t: var Test): NimNode =
  ## compose a try/except/finally block around a test
  var skipping = bindSym"SkipError"
  var failing = bindSym"FailError"
  var assertion = bindSym"AssertionDefect"
  var catchall = bindSym"Exception"
  var e1 {.used.} = genSym(nskLet, "e")
  var e2 {.used.} = genSym(nskLet, "e")
  var e3 {.used.} = genSym(nskLet, "e")
  var e4 {.used.} = genSym(nskLet, "e")
  result = nnkTryStmt.newTree(t.n,
           nnkExceptBranch.newTree(infix(failing, "as", e4), t.badassert(e4)),
           nnkExceptBranch.newTree(infix(skipping, "as", e1), t.skipped(e1)),
           nnkExceptBranch.newTree(infix(assertion, "as", e2), t.badassert(e2)),
           nnkExceptBranch.newTree(infix(catchall, "as", e3), t.exception(e3)),
           nnkFinally.newTree(t.postTest))

proc makeTest(n: NimNode; name: string): Test =
  ## we're given `n`, which is a block: or something, and a test name.
  ## we compose a test that performs timings, measures memory, catches
  ## exceptions, and reports compilation failures.
  assert not n.isNil
  # the test counter is used to, uh, count tests
  inc testCount
  # this is our test object; note that `Test.n` hasn't been added yet
  result = Test(name: name, orig: n, number: testCount)
  #let beuno {.used.} = genSym(nskLabel, "beuno")  # all good, bro
  #let arrrg {.used.} = genSym(nskLabel, "arrrg")  # bad news, pal

  # we've stored the original code in the Test object, so now we
  # copy the input and put it into a new statement list; `n` will
  # hold the code that we'll actually run to instrument the test
  result.n = copyNimTree(n).newStmtList

  # if the input ast is testable,
  if n.kind in testable:
    # we will add a "success" event to the bottom of our instrumentation
    result.n.add result.success

    # make note of the global clock time at the beginning of the test
    insert(result.n, 0, newAssignment(bindSym"clock",
                                      newCall(bindSym"epochTime")))

    # but first perform a garbage collection or whatever, so our
    # memory figures might be kinda sorta useful, and store it globally
    insert(result.n, 0, newAssignment(bindSym"memory",
                                      newCall(bindSym"quiesceMemory",
                                              newLit"")))

    # wrap all the instrumentation to catch any exceptions
    result.n = result.wrapExcept

  else:
    # it's not testable; we'll indicate that it worked (what else?)
    when defined(release):
      result.status = Okay

    # output the status in any event; otherwise there will be no output
    result.n.add result.output(newLit result.name)

    # a test that wasn't actually testable doesn't deserve a counter
    dec testCount

  # wrap it into `when compiles(original): test else: compilerr`
  # this'll allow tests that don't compile to produce useful output
  when not defined(release):
    result.n = nnkWhenStmt.newTree(
      nnkElifBranch.newTree(
        newCall(ident"compiles",
                nnkBlockStmt.newTree(genSym(nskLabel, "compiles"),
                                     newStmtList(result.orig))),
        result.n),
      nnkElse.newTree(result.compilerr))

proc rewriteTestBlock(n: NimNode): NimNode =
  ## rewrite test "something": ... as block: ## something ...
  assert not n.isNil
  result = n
  if n.kind == nnkCommand and len(n) == 3:
    if n[0].kind == nnkIdent and eqIdent(n[0], "test"):
      if n[1].kind == nnkStrLit and n[2].kind == nnkStmtList:
        let name = newCommentStmtNode(n[1].strVal)
        result = nnkBlockStmt.newTree(newEmptyNode(), newStmtList(name, n[2]))

proc findName(n: NimNode; index: int): string =
  ## generate a name for a test block
  assert not n.isNil
  if len(n) == 2 and n.kind == nnkBlockStmt:
    # grab the body of the block,
    var body = n.last
    # and the first node
    var head = if len(body) > 0: body[0] else: newEmptyNode()
    # pick the best of these,
    result = if body.kind == nnkStmtList and head.kind == nnkCommentStmt:
      head.strVal.splitLines(keepEol = false)[0]
    elif n[0].kind in {nnkIdent, nnkSym}:
      n[0].strVal.replace("_", " ")
    elif body.kind == nnkStmtList and head.kind in RoutineNodes:
      head.name.strVal.replace("_", " ")
    else:
      repr(n)
  else:
    result = repr(n)

macro testes*(tests: untyped) =
  ## For a good time, put each test in a `block:` underneath the `testes`.
  ## You can specify test names using `##` comment statements, or block
  ## syntax like that of `unittests`: `test "my test name": assert true`
  try:
    result = newStmtList()

    # windows cmd / powershell color support
    when defined(windows):
      result.add(nnkDiscardStmt.newTree newCall(ident"execShellCmd", newLit""))

    for index, n in pairs(tests):
      var n = n.rewriteTestBlock
      var test: Test
      if n.kind == nnkCommentStmt:
        let prefix = $lineNumStyle & "## " & $commentStyle
        result.add output(infix(prefixLines(n, prefix),
                                "&", newLit(resetStyle.string)))
      else:
        let name = findName(n, index)
        test = makeTest(n, name)
        result.add test.n
  finally:
    result.add reportResults()

# unused code that may move back into service
when false:
  proc massageLabel(n: NimNode): NimNode =
    assert not n.isNil
    case n.kind
    of nnkStrLit:
      result = genSym(nskLabel, $n.strVal)
    of nnkIntLit:
      result = genSym(nskLabel, $n.intVal)
    else:
      result = n

  proc shortenRepr(n: NimNode): string =
    let splat = repr(n).splitLines
    if len(splat) == 1:
      result = splat[0]
    else:
      result = splat[0] & " ⮠..."

  proc countComments(n: NimNode): int =
    assert not n.isNil
    if n.kind == nnkStmtList:
      for _ in items(n):
        if _.kind == nnkCommentStmt:
          result.inc len(_.strVal.splitLines())
        else:
          break

when isMainModule:
  import std/[strutils, tables, os, osproc]

  ##
  ## it's crude because it's converted from nimscript
  ##

  const
    directory = "tests"

  type
    Compilers = enum c, cpp
    Optimizations = enum debug, release, danger
    Models = enum refc, markAndSweep, arc, orc

  # set some default matrix members
  var opt = {debug: @["--stackTrace:on"]}.toTable
  var cp = @[c]
  # the default gc varies with version
  var gc =
    when (NimMajor, NimMinor) >= (1, 2):
      {arc}
    else:
      {refc}
  var
    hints = @["--hint[Cc]=off", "--hint[Link]=off", "--hint[Conf]=off",
              "--hint[Processing]=off", "--hint[Exec]=off"]
    defaults = @["--forceBuild:on",
                 "--path:" & quoteShell(parentDir directory)]

  # remote ci expands the matrix
  if getEnv("GITHUB_ACTIONS", "false") == "true":
    cp.add cpp                  # add cpp
    gc.incl refc                # add refc
    gc.incl markAndSweep        # add markAndSweep
    if arc in gc:               # add orc if arc is available
      if (NimMajor, NimMinor) >= (1, 4):  # but 1.2 has infinite loops!
        gc.incl orc
    for o in {release, danger}: # add other optimization levels
      opt[o] = opt.getOrDefault(o, @[]) & @["--define:" & $o]

  proc attempt(cmd: string): int =
    ## attempt execution of a random command; returns the exit code
    checkpoint "$ " & cmd
    try:
      result = execCmd cmd
    except OSError:
      result = 1

  proc perform(fn: string) =
    ## build and run a test according to matrix; may quit with exit code
    let pattern = "nim $1 --gc:$2 $3 --run " & hints.join(" ") & " $4"
    for opt, options in opt.pairs:
      var options = defaults & options
      for gc in gc.items:
        for cp in cp.items:
          let run = pattern % [$cp, $gc, options.join(" "), fn]
          let code = attempt run
          if code != 0:
            case $NimMajor & "." & $NimMinor
            of "1.4":
              if gc > orc:
                continue
            of "1.2":
              if gc > arc:
                continue
            else:
              discard
            # i don't care if cpp works anymore
            if cp != cpp:
              checkpoint "test `" & run & "` failed; compiler:"
              flushFile stderr  # hope we beat the compiler's --version
              discard execCmd "nim --version"
              quit code

  proc ordered(directory: string): seq[string] =
    ## order a directory of test files usefully
    # collect the filenames
    for test in directory.walkDirRec(yieldFilter = {pcFile, pcLinkToFile}):
      if test.extractFilename.startsWith("t") and test.endsWith(".nim"):
        result.add test

    # sort them by age, recently-changed first
    proc age(path: string): Time =
      getFileInfo(path, followSymlink = true).lastWriteTime
    proc byAge(a, b: string): int = system.cmp(a.age, b.age)
    result.sort(byAge, Descending)

  for test in ordered directory:
    perform test

{.pop.}
