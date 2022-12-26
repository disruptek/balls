import std/options
import std/times
import std/os
import std/sequtils
import std/strutils except align, alignLeft
from std/unicode import align, alignLeft
import std/macros
import std/colors

import grok
import grok/mem
import grok/time
import grok/kute
import ups/sanitize

import balls/spec
import balls/style

export FailError, SkipError, ExpectedError

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

proc combineLiterals(n: NimNode): NimNode =
  ## merges "foo" & "bar" into "foobar"

  proc collapser(n: NimNode): NimNode =
    ## cleaning up useColor conditionals
    case n.kind
    of nnkIfStmt:
      if n.len == 2:
        # cheating here
        if {n[0].kind, n[1].kind} == {nnkElifBranch, nnkElse}:
          if n[0].len == 2:
            if n[0][0].len == 1 and n[1].len == 1:
              if n[0][0].kind == nnkCall:
                if n[0][0][0].kind == nnkSym:
                  if n[0][0][0].strVal == "useColor":
                    if n[0][1] == n[1][0]:
                      result = n[1][0]
    else:
      discard

  template isEmptyStringLiteral(n: NimNode): bool =
    n.kind == nnkStrLit and n.strVal == ""

  proc isSimpleConcatenation(n: NimNode): bool =
    if n.kind in {nnkCall, nnkInfix} and n.len == 3:
      result = n[0].kind in {nnkIdent, nnkSym} and n[0].strVal == "&"

  proc count(n: NimNode): int =
    result = 1
    for child in n.items:
      result += count child

  proc rewriteLiteralConcatenation(n: NimNode): NimNode =
    ## merging concatenations including literals
    if n.isSimpleConcatenation:
      var a = rewriteLiteralConcatenation n[1]
      var b = rewriteLiteralConcatenation n[2]
      # &(exp, "") -> exp
      if a.isEmptyStringLiteral:
        result = b
      # &("", exp) -> exp
      elif b.isEmptyStringLiteral:
        result = a
      # &("foo", "bar") -> "foobar"
      elif {a.kind, b.kind} == {nnkStrLit}:
        result = newLit(a.strVal & b.strVal)
      # &("foo", &("bar", exp)) -> &("foobar", exp)
      elif a.kind == nnkStrLit and b.isSimpleConcatenation:
        if b[1].kind == nnkStrLit:
          result = newCall(ident"&", newLit(a.strVal & b[1].strVal), b[2])
      # &(&(exp, "foo"), "bar") -> &(exp, "foobar")
      elif b.kind == nnkStrLit and a.isSimpleConcatenation:
        if a[2].kind == nnkStrLit:
          result = newCall(ident"&", a[1], newLit(a[2].strVal & b.strVal))

  result = n
  result = rewrite result: collapser
  result = rewrite result: rewriteLiteralConcatenation

proc comment(n: NimNode): NimNode =
  ## render a comment with the given stringish node
  infix(lineNumStyle & newLit"## ", "&", n)

var testResults = newSeq[int](1 + ord(high StatusKind))  # result totals

proc incResults(test: Test): NimNode =
  newCall ident"inc":
    nnkBracketExpr.newTree(bindSym"testResults", newLit test.status.ord)

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
  of nnkStmtList:
    ss = s
  else:
    ss = newStmtList s
  for line in ss.items:
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
  ## Like `echo`, but outputs to `stdmsg()` with the other test output.
  noclobber:
    when defined(js):
      echo ss.join(" ")
    else:
      stdmsg().writeLine ss.join(" ")

proc output(n: NimNode): NimNode =
  assert not n.isNil
  result = newCall(bindSym"checkpoint", combineLiterals n)

proc output(t: Test; n: NimNode): NimNode =
  assert not n.isNil
  let prefixer = bindSym"prefixLines"
  result = output newCall(prefixer, dollar(n), newLit($t.status & " "))

proc output(test: Test; styling: Styling; n: NimNode): NimNode {.used.} =
  assert not n.isNil
  let prefixer = bindSym"prefixLines"
  result = newCall(prefixer, dollar(n), newLit($test.status & " "))
  result = test.output(styling & result)

macro report(n: string) =
  ## render a multi-line comment
  var prefix = $lineNumStyle & "## " & $commentStyle
  var postfix = newLit resetStyle.string
  result =
    nnkIfStmt.newTreeFrom n:
      nnkElifExpr.newTreeFrom n:
        newCall bindSym"useColor"
        output infix(prefixLines(n, prefix), "&", postfix)
      nnkElse.newTree:
        output prefixLines(n, "## ")

proc report(n: NimNode): NimNode =
  result = getAst(report n)

proc niceKind(n: NimNode): NimNode =
  expectKind(n, nnkSym)
  let kind = $n.symKind
  result = newLit toLowerAscii(kind[3..^1])

proc localPath(fn: string): string =
  ## a somewhat verbose impl due to necessity
  when nimvm:
    when (NimMajor, NimMinor) == (1, 4):
      result = fn
    else:
      result = relativePath(fn, getProjectPath())
  else:
    when defined(js):
      block:
        when (NimMajor, NimMinor) >= (1, 4):
          try:
            result = relativePath(fn, getCurrentDir())
            break
          except ValueError:
            # "specified root is not absolute"; cwd probably unavailable
            discard
        result = relativePath(fn, getProjectPath())
    else:
      result = relativePath(fn, getCurrentDir())

proc renderFilename(s: LineInfo): string =
  "$1$3$2$1" % [ $resetStyle,
                 localPath($s.filename),
                 $viaFileStyle ]

proc renderFilenameAndLine(s: LineInfo): string =
  "$1$2$3$1$4 line $5$1" % [ $resetStyle,
                 $viaFileStyle, localPath($s.filename),
                 $commentStyle, $s.line ]

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
          when defined(gcArc) or defined(gcOrc):
            newLit"(unsupported on arc/orc)"
          else:
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

proc fromFileGetLine(file: string; line: int): string =
  ## TODO: optimize this expensive fetch
  when defined(js):
    result = "(unsupported on js)"
  else:
    if file.fileExists:
      let lines = toSeq lines(file)
      result = lines[line - 1]
    else:
      #result = "(not found: $#)" % [ file ]
      result = "(file not found)"

proc findWhere(s: string; p: string; into: var string): bool {.used.} =
  ## find the location of a substring and, if found, produce empty prefix
  result = s.count(p) == 1
  if result:
    into = spaces(s.find(p))

proc renderStack(prefix: string; stack: seq[StackTraceEntry]) =
  ## stylishly render a stack trace
  var cf: string
  var result: seq[string]
  for s in stack.items:
    if cf != $s.filename:
      cf = $s.filename
      result.add renderFilename(s)
    let code = fromFileGetLine(cf, s.line)
    let line = align($s.line, 5)
    result.add "$1$5$2 $6$3  $7# $4()$1" % [ $resetStyle,
      line, code, $s.procname, $lineNumStyle, $sourceStyle, $viaProcStyle ]
  checkpoint result.join("\n").prefixLines prefix & emojiStack

proc renderTrace(t: Test; n: NimNode = nil): NimNode =
  ## output the stack trace of a test, and perhaps that of any exception
  when defined(js):
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
  result = t.output(repr(node).numberLines(info.line).prefixLines emojiSource)

proc setExitCode(t: Test; code = QuitFailure): NimNode =
  let setResult = bindSym"setBallsResult"
  when false:
    let isAtty = bindSym"isAtty"
    # if not isAtty(stderr): setResult(code)
    result = newIfStmt((prefix(newCall(isAtty, ident"stderr"), "not"),
                        newCall(setResult, code.newLit)))
  else:
    # because the test runner needs to measure success using exit code,
    # we need to exit with a code regardless of tty condition
    result = newCall(setResult, code.newLit)

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
      except ExpectedError as e:
        fail e.msg
      except CatchableError as e:
        checkpoint "$#: $#" % [ $e.name, e.msg ]
        fail "expected $# but caught $#" % [ $exception, $e.name ]
      raise newException ExpectedError:
        "expected $# exception" % [ $exception ]

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
    combineLiterals nestList(ident"&", legend)

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

  when defined(danger):
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
  var catchall =
    when compiles(CatchableError):
      bindSym"CatchableError"
    else:
      bindSym"Exception"
  var e1 {.used.} = genSym(nskLet, "e")
  var e2 {.used.} = genSym(nskLet, "e")
  var e3 {.used.} = genSym(nskLet, "e")
  var e4 {.used.} = genSym(nskLet, "e")
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

    when not defined(js):
      # ensure that we flush streams on exit
      add suite:
        bindSym"addExitProc".newCall bindSym"flushStreams"

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
    add result:
      newBlockStmt:
        suite

macro test*(name: string; body: untyped) =
  ## A compatibility shim for adapting `std/unittest` syntax.
  newBlockStmt(genSym(nskLabel, name.strVal), body)

when isMainModule:
  import balls/runner

  # search the tests subdirectory for tests; fall back to current directory
  main "tests", fallback = true
