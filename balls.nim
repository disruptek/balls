import std/colors
import std/genasts
import std/macros
import std/options
import std/os
import std/strutils except align, alignLeft
import std/tables
import std/times
from std/unicode import align, alignLeft

import pkg/grok
import pkg/grok/time
import pkg/grok/kute
import pkg/ups/sanitize

import balls/spec
import balls/style
export checkpoint

export FailError, SkipError

const LNcols = 5  # how many columns to reserve for line numbers

type
  Rewrite = proc(n: NimNode): NimNode

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

when ballsAuditTimeSpace:
  var clock: float          # pre-test time
  var memory: int           # pre-test memory

proc rewrite(n: NimNode; r: Rewrite): NimNode =
  ## perform a recursive rewrite (at least once) using the given mutator
  result = r(n)
  if result.isNil:
    result = copyNimNode n
    for kid in n.items:
      result.add rewrite(kid, r)
    let second = r(result)
    if not second.isNil:
      result = second

proc filter(n: NimNode; f: Rewrite): NimNode =
  ## perform a recursive rewrite (only once) using the given mutator
  result = f(n)
  if result.isNil:
    result = copyNimNode n
    for kid in n.items:
      result.add filter(kid, f)
  result = n

# result totals whatfer legend depiction
proc newtestResults(): seq[int] {.compileTime.} =
  newSeq[int](1 + ord(high StatusKind))

# silly, i know
when defined(nimscript):
  var testResults {.compileTime.} = newTestResults()
else:
  var testResults = newTestResults()

proc incResults(test: Test): NimNode =
  newCall ident"inc":
    nnkBracketExpr.newTree(bindSym"testResults", newLit test.status.ord)

proc `status=`(t: var Test; s: StatusKind) {.used.} =
  system.`=`(t.status, max(t.status, s))

proc prefixLines(s: string; p: string): string =
  ## prefix each line of multiline input with the given string
  for line in items(splitLines(s, keepEol = true)):
    result.add p & line

proc numberLinesCT(s: string; first = 1): NimNode =
  ## prefix each line of multiline input with a rendered line number
  result = newStmtList()
  for n, line in pairs(splitLines(s, keepEol = true)):
    var ln = lineNumStyle & align($(n + first), LNcols).newLit
    ln = infix(ln, "&", "  ".newLit)
    ln = infix(ln, "&", sourceStyle & line.newLit)
    result.add ln

proc numberLinesRT(s: string; first = 1): seq[string] =
  ## prefix each line of multiline input with a rendered line number
  for n, line in pairs(splitLines(s, keepEol = true)):
    result.add:
      "$1$2$3  $4$5" % [ $resetStyle, $lineNumStyle, align($(n + first), LNcols),
                        $sourceStyle, line ]

proc output(t: Test; n: NimNode): NimNode =
  assert not n.isNil
  let prefixer = bindSym"prefixLines"
  result = output newCall(prefixer, n, newLit($t.status & " "))

proc output(test: Test; styling: Styling; n: NimNode): NimNode {.used.} =
  assert not n.isNil
  let prefixer = bindSym"prefixLines"
  result = newCall(prefixer, n, newLit($test.status & " "))
  result = test.output(styling & result)

proc report(n: NimNode): NimNode =
  result = getAst(report n)

proc niceKind(n: NimNode): NimNode =
  expectKind(n, nnkSym)
  let kind = $n.symKind
  result = newLit toLowerAscii(kind[3..^1])

proc renderFilename(s: StackTraceEntry): string =
  renderFilename LineInfo(filename: $s.filename, line: s.line)

proc revealSymbol(n: NimNode): NimNode =
  ## produce a useful emission for the symbol
  let
    reveal = bindSym"report"
    #impl = getTypeImpl n        # just
    sym = getImpl n              # the
    #typ = getType n             # usual
    #inst = getTypeInst n        # suspects

  case n.symKind
  of nskEnumField:
    result = reveal.newCall:
      bindSym"&".nestList:
        # checkpoint kind: enum = FooValue
        newStmtList: [
          niceKind n,
          newLit" = ",
          newLit repr(n),
        ]
  of nskVar, nskLet, nskParam, nskForVar, nskResult, nskConst:
    let typ = getType n
    result = reveal.newCall:
      bindSym"&".nestList:
        # checkpoint kind: x = 32
        newStmtList: [
          niceKind n,
          newLit" ",
          newLit repr(n),
          newLit": ",
          newLit repr(typ),
          newLit" = ",
          newCall(ident"repr", n)
        ]
  of nskProc, nskFunc:
    result = reveal.newCall:
      bindSym"&".nestList:
        # checkpoint proc: x location
        newStmtList: [
          niceKind n,
          newLit" ",
          newLit repr(n),
          newLit" from ",
          newLit renderFilenameAndLine(sym.lineInfoObj),
        ]
  else:
    result = reveal.newCall:
      bindSym"&".nestList:
        # checkpoint kind: x
        newStmtList: [
          niceKind n,
          newLit" ",
          newLit repr(n),
          newLit": ",
          newLit"(afraid to check type or value)"
        ]

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

proc checkOne(condition: NimNode; message: NimNode): NimNode =
  ## generate a simple check statement with optional exception message
  var message =
    if message.kind == nnkStrLit and message.strVal == "":
      newLit condition.repr
    else:
      message

  # collect the symbols we want to display
  var symbolReport = newStmtList()

  # use a filter with a side-effect to populate the list
  proc showSymbols(n: NimNode): NimNode {.used.} =
    if n.kind == nnkSym:
      add symbolReport:
        revealSymbol n

  # run the filter to pull out the interesting symbols
  when not defined(ballsNoDump):
    discard filter(condition, showSymbols)

  # if not condition:
  #   reveal symbols
  #   raise newException(AssertionDefect, "message")
  var clause = newStmtList symbolReport
  add clause:
    nnkRaiseStmt.newTree:
      nnkObjConstr.newTree [
        nnkRefTy.newTree(bindSym"FailError"),
        nnkExprColonExpr.newTree(ident"msg", message),
      ]

  result = newIfStmt (ident"not".newCall condition, clause)

macro check*(body: bool; message: string = "") =
  ## Check a single expression; raises an AssertionDefect in the event
  ## that the expression is `false` regardless of `assertions` settings.
  ## Specify a custom `message` a la `assert`.
  result = checkOne(body, message)

macro check*(message: string; body: untyped) =
  ## Check one or more expressions in a block; raises an AssertionDefect
  ## in the event that an expression is `false` regardless of `assertions`
  ## settings.  Specify a custom `message` a la `assert`.
  var comment = message
  body.expectKind nnkStmtList
  result = newStmtList()
  for child in body.items:
    case child.kind
    of nnkCommentStmt:
      # reset the comment string for subsequent tests in the block
      comment = newCall(ident"&", message, newLit ": " & child.strVal)
    else:
      add result:
        newCall(bindSym"check", child, comment)

proc success(t: var Test): NimNode =
  ## what to do when a test is successful
  t.status = Pass
  result = newStmtList()
  result.add t.incResults
  result.add t.output(successStyle & newLit(t.name))

type
  LN = int
  FN = cstring
  LineMap = Table[LN, string]
  FileLineMap = Table[FN, Option[LineMap]]
# if you're here because filename isn't a cstring or line number isn't an int... 😦

proc getLineMapFromFile(file: StackTraceEntry.filename): Option[LineMap] =
  when not defined(js):
    let file = $file
    if file.fileExists:
      result = some default(LineMap)
      var i: LN
      inc i  # files start with line 1
      for line in file.lines:
        result.get[i] = line
        inc i

proc findWhere(s: string; p: string; into: var string): bool {.used.} =
  ## find the location of a substring and, if found, produce empty prefix
  result = s.count(p) == 1
  if result:
    into = spaces(s.find(p))

proc renderStackEntry*(s: StackTraceEntry; lineMap: Option[LineMap]): string =
  ## render a stacktrace entry nicely; "progressive enhancement"
  var source: Option[string]
  if lineMap.isSome:
    if s.line in lineMap.get:
      source = some lineMap.get[s.line]
  var bland {.used.}: string
  var colorized {.used.}: string = $resetStyle
  if source.isSome:
    bland.add "$1  " % [ get source ]
    colorized.add "$1$2  " % [ $sourceStyle, get source ]
  if 0 < len $s.procname:
    bland.add "# $1()" % [ $s.procname ]
    colorized.add "$1# $2$3()" % [ $lineNumStyle, $viaProcStyle, $s.procname ]
  if 0 == s.line:
    bland = align("?", LNcols) & "  " & bland
    colorized = align("?", LNcols) & "  " & colorized
  else:
    bland = numberLinesRT(bland, s.line)[0]
    colorized = numberLinesRT(colorized, s.line)[0]
  colorized.add $resetStyle
  result = withColor(bland, colorized , bland)

proc renderStack(prefix: string; stack: seq[StackTraceEntry]) =
  ## stylishly render a stack trace
  var cf: string
  var result: seq[string]
  var fileLineMap: FileLineMap
  let prefix = prefix & " " & emojiStack
  for s in stack.items:
    # did the filename change?
    if cf != $s.filename:
      cf = $s.filename
      # get any missing line map for the file
      if s.filename notin fileLineMap:
        fileLineMap[s.filename] = getLineMapFromFile(s.filename)
      # add a line rendering the changed filename
      result.add " " & renderFilename(s)
      # add the source code rendered with line number
      result.add renderStackEntry(s, fileLineMap[s.filename])
  checkpoint prefixLines(result.join("\n"), prefix)

proc renderTrace(t: Test; n: NimNode = nil): NimNode =
  ## output the stack trace of a test, and perhaps that of an exception
  when defined(js) or defined(nimscript):
    result = newEmptyNode()
  else:
    var renderStack = bindSym"renderStack"
    var getStack = newCall(bindSym"getStackTraceEntries")
    if not n.isNil:
      getStack.add n  # get the exception's stacktrace
    result = newIfStmt((newCall(bindSym"stackTraceAvailable"),
                        newCall(renderStack, newLit($t.status), getStack)))

proc renderSource(t: Test): NimNode =
  ## strip the first comment, include the remainder
  var node = copyNimTree(t.code)
  var info = lineInfoObj(t.code)
  if len(node) > 0:
    if node[0].kind == nnkCommentStmt:
      let dropFirst = node[0].strVal.splitLines(keepEol = true)[1..^1].join("")
      node[0] = newCommentStmtNode(dropFirst)
  result = t.output(repr(node).numberLinesCT(info.line).prefixLines emojiSource)

when defined(nimscript):
  # under nimscript, we don't have a good way to enqueue a result code
  proc setExitCode(t: Test; code = QuitFailure): NimNode =
    let warning =
      t.output: newLit"(balls exits on first failure under nimscript)"
    genAstOpt({}, warning, code):
      if code != QuitSuccess:
        warning
        quit code
elif defined(js):
  # we don't actually call the exit routine...
  # proc processExit(code: cint = 0) {.importjs: "process.exit(#)".}
  var exitCode {.importjs: "process.$1".}: cint
  proc setExitCode(t: Test; code = QuitFailure): NimNode =
    newAssignment(bindSym"exitCode", code.newLit)
elif false:
  # other backends use the modern get|set-ProgramResult routines
  import std/exitprocs
  proc setExitCode(t: Test; code = QuitFailure): NimNode =
    genAstOpt({}, code):
      setProgramResult max(code, getProgramResult())
else:
  import std/atomics

  type
    Grenade = object
      exitCode: Atomic[int]

  proc `=destroy`(grenade: var Grenade) =
    quit grenade.exitCode.load

  var grenade = Grenade()

  proc setExitCode(t: Test; code = QuitFailure): NimNode =
    genAstOpt({}, code=code.ord, g=bindSym"grenade"):
      g.exitCode.store(max(g.exitCode.load, code))

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
  result =
    nnkStmtList.newTreeFrom n:
      t.incResults
      t.output:
        nestList bindSym"&":
          nnkStmtList.newTreeFrom n:
            skippedStyle & t.name.newLit
            newLit": "
            n.dotMsg

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

template expect*(exception: typed; body: untyped) =
  ## Fails the test if an expected exception is not raised in the body.
  when exception isnot CatchableError:
    raise newException(Defect, "supply a CatchableError type")
  else:
    block expected:
      try:
        body
      except exception:
        break expected
      fail "expected $# exception" % [ $exception ]

proc reportResults(): NimNode =
  ## produce a small legend showing result totals
  var checkpoint = bindSym"checkpoint"
  var results = bindSym"testResults"
  var legend = newStmtList()
  add legend:
    comment resultsStyle & newLit($totalTests() & " tests  ")
  for status in items(StatusKind):
    add legend, newLit"  "             # space the legend contents
    # essentially, Pass -> results[2]
    let brack = nnkBracketExpr.newTree(results, newLit status.ord)
    add legend:
      nnkIfStmt.newTreeFrom nil:
        nnkElifBranch.newTreeFrom nil:
          infix(brack, ">", 0.newLit)               # if results[2] > 0:
          infix(newLit $status, "&", dollar brack)  #   "🟢" & $results[2]
        nnkElse.newTree:                            # else:
          newLit""                                  #   ""
  # the result is a concatenation of the above expressions
  result = checkpoint.newCall:
    nestList(ident"&", legend)

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
  ## copy compile-time Test into runtime Test constructor
  let typ = bindSym"Test"
  result = nnkObjConstr.newTree(typ)
  for name, value in fieldPairs(test):
    when value isnot ref:
      result.add composeColon(ident(name), value)

when defined(danger):
  proc pad(n: NimNode; size: int): NimNode =
    let align = bindSym"align"
    result = newCall(align, newCall(bindSym"$", n), size.newLit)

  proc humanize(n: NimNode): NimNode =
    ## convert bytes to human-readable form
    template abs(n: NimNode): NimNode =
      newCall(bindSym"abs", n)
    template kute(n: untyped): NimNode =
      newCall(bindSym"$", newCall(bindSym"Kute", n))
    result =
      nnkIfExpr.newTreeFrom n:
        nnkElifBranch.newTreeFrom n:
          infix(n, ">", 0.newLit)                # if n > 0:
          infix(newLit"+", "&", kute(n))         #   "+" & kute(n)
        nnkElifBranch.newTreeFrom n:
          infix(n, "==", 0.newLit)               # elif n == 0:
          newLit""                               #   ""
        nnkElse.newTree:                         # else:
          infix(newLit"-", "&", kute(abs n))     #   "-" & kute(abs n)

proc postTest(test: Test): NimNode =
  ## run this after a test has completed
  let temp = genSym(nskVar, "test")
  result = newStmtList:
    newVarStmt(temp, ctor test)

  when ballsAuditTimeSpace:
    let tempClock = newDotExpr(temp, ident"clock")
    let tempMem = newDotExpr(temp, ident"memory")

    # record the duration
    result.add newAssignment(tempClock, infix(newCall(bindSym"epochTime"),
                                              "-", bindSym"clock"))
    # record the memory
    let quiesce = newCall(bindSym"quiesceMemory", newLit"")
    add result:
      tempMem.newAssignment:        # tempMem =
        quiesce.infix "-":          # quiesceMemory("") -
          bindSym"memory"           # memory

    let billion = newLit 1_000_000_000
    let nano = bindSym"initDuration".newCall:   # initDuration:
      nnkExprEqExpr.newTree ident"nanoseconds": # nanoseconds =
        ident"int".newCall:                     # int
         tempClock.infix "*", billion           # (clock * 1,000,000,000)

    # compose the status line
    var text = newStmtList()
    add text:        # the test number with extra styling
      testNumStyle & pad(newLit $test.number, 5)
    add text:        # the change in memory footprint after the test
      pad(humanize tempMem, 30)
    add text:        # a short duration representing how long the test took
      pad(bindSym"shortDuration".newCall nano, 20)

    # output the text as a single concatenated comment
    add result:
      output:
        comment nestList(ident"&", text)

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

template skip*(msg = "skipped", body: untyped) =
  ## Manually skips the remainder of the current test and
  ## stops macro processing.
  skip(msg)
  when false:
    body

proc fail*(msg = "failed") =
  ## Manually fails the current test.
  raise newException(FailError, msg)

proc wrapExcept(t: var Test): NimNode =
  ## compose a try/except/finally block around a test
  var skipping = bindSym"SkipError"
  var failing = bindSym"FailError"
  var assertion = bindSym"AssertionDefect"
  var catchall = bindSym"CatchableError"
  var e1 {.used.} = genSym(nskLet, "skippedErr")
  var e2 {.used.} = genSym(nskLet, "badAssertErr")
  var e3 {.used.} = genSym(nskLet, "catchableErr")
  var e4 {.used.} = genSym(nskLet, "failureErr")
  result =
    nnkTryStmt.newTreeFrom t.code:
      # the body of the try statement is the instrumented test
      t.node
      # test failures are implemented as exceptions
      nnkExceptBranch.newTreeFrom t.code:
        infix(failing, "as", e4)
        badassert(t, e4)
      # test skipping is implemented via exceptions
      nnkExceptBranch.newTreeFrom t.code:
        infix(skipping, "as", e1)
        skipped(t, e1)
      # failed assertions from `assert` are caught here
      nnkExceptBranch.newTreeFrom t.code:
        infix(assertion, "as", e2)
        badassert(t, e2)
      # random exceptions in the tests are still catchable
      nnkExceptBranch.newTreeFrom t.code:
        infix(catchall, "as", e3)
        exception(t, e3)
      # finally, perform any post-test reporting
      nnkFinally.newTree:
        postTest t

proc makeTest(n: NimNode; name: string): Test =
  ## we're given `n`, which is a block: or something, and a test name.
  ## we compose a test that performs timings, measures memory, catches
  ## exceptions, and reports compilation failures.
  assert not n.isNil
  # initialize the new Test object using the name and original code
  result.init(name = name, code = n)

  # if the input ast is testable,
  if n.kind in testable:
    # add a "success" event to the bottom of our instrumentation; any
    # failure will raise an exception before reaching this code
    result.node.add:
      success result

    when ballsAuditTimeSpace:
      # make note of the global clock time at the beginning of the test
      insert result.node, 0:
        bindSym"clock".newAssignment:      # clock =
          bindSym"epochTime".newCall       # epochTime()

      # but first perform a garbage collection or whatever, so our memory
      # figures might be kinda sorta useful, and store memory use globally
      insert result.node, 0:
        bindSym"memory".newAssignment:              # memory =
          bindSym"quiesceMemory".newCall newLit""   # quiesceMemory("")

    # and before you do that, run the setup clause
    insert result.node, 0:
      newCall(ident"setup")

    # and remember to tear it down afterwards
    result.node.add:
      newCall(ident"teardown")

    # wrap all the instrumentation to catch any exceptions
    result.node = wrapExcept result

  else:
    # output the status in any event; otherwise there will be no output
    add result.node:
      output result:
        newLit result.name

  # wrap it into `when compiles(original): test else: compilerr`
  # this'll allow tests that don't compile to produce useful output
  when not defined(release):
    # check compilation within a block:
    let compilationOkay =
      ident"compiles".newCall:
        nnkBlockStmt.newTree(newEmptyNode(), result.code)
    result.node =
      nnkWhenStmt.newTreeFrom n:
        # successful compilation invokes the test `node`
        nnkElifBranch.newTree(compilationOkay, result.node)
        # compilation failure invokes compilerr on the Test
        nnkElse.newTree(compilerr result)

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

proc scopedTemplate(n: NimNode): bool =
  ## true if we should redefine the template in a new scope;
  ## sets the name to match the discovered identifier.
  const supported = ["setup", "teardown"]
  if n.kind == nnkCall and n.len == 2:
    if n[0].kind == nnkIdent:
      if n[0].strVal in supported:
        result = true

macro suite*(name: string; tests: untyped) =
  ## Put each test in a `block:` underneath the named suite. You can specify
  ## test names using `##` comment statements, or block syntax like that
  ## of `unittests`: `test "my test name": check true`
  result = newStmtList report(name)

  var suite =
    newStmtList:
      # the default setup() and teardown()
      quote:
        template setup(args: varargs[untyped]): untyped {.used.} = discard
        template teardown(args: varargs[untyped]): untyped {.used.} = discard

  try:
    # windows cmd / powershell color support
    when defined(windows):
      add result:
        nnkDiscardStmt.newTree:
          bindSym"execShellCmd".newCall newLit""

    var parent = suite
    for index, n in tests.pairs:
      if scopedTemplate n:
        # rewriting setup: or teardown: into templates
        # that replace those of prior scope...
        let name = n[0]
        let body = n[1]
        var child =
          newBlockStmt:
            newStmtList:
              quote:
                template `name`(): untyped = `body`
        parent.add child
        parent = child.last
      else:
        # rewriting test blocks as per usual...
        var child = newStmtList()
        parent.add child

        var test: Test
        var n = n.rewriteTestBlock
        if n.kind == nnkCommentStmt:
          add child: report n            # report comments
        else:
          let name = findName(n, index)  # discover the test name
          test = makeTest(n, name)       # create the test
          add child: test.node           # add the test body

        parent = child

  finally:
    add suite:
      reportResults()

    when not defined(js):
      # ensure that we flush streams on exit
      add suite:
        newCall bindSym"flushStreams"

    add result:
      newBlockStmt:
        suite

macro test*(name: string; body: untyped) =
  ## A compatibility shim for adapting `std/unittest` syntax.
  newBlockStmt(genSym(nskLabel, name.strVal), body)

when isMainModule:
  import balls/runner

  proc nonOptionArguments(args: openArray[string]): seq[string] =
    for arg in args.items:
      if not arg.startsWith("-"):
        result.add arg

  let patterns = nonOptionArguments commandLineParams()
  if 0 < patterns.len:
    main(patterns)        # search using the provided patterns,
  else:                   # or the default pattern
    main([testPattern])
