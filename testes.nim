import std/os
import std/sequtils
import std/terminal
import std/sugar
import std/strutils
import std/macros
import std/colors

when (NimMajor, NimMinor) >= (1, 3):
  import std/exitprocs
else:
  proc setProgramResult(q: int) =
    programResult = q

import cutelog

export sugar, strutils, macros, cutelog

const
  statements = {
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

var
  depth {.compileTime.}: int

type
  SkipError = object of CatchableError
  StatusKind = enum
    None = "⚫"
    Info = "🔵"
    Okay = "🟢"
    Skip = "❔"
    Part = "🟡"
    Fail = "🔴"
    Died = "💥"
    Oops = "⛔"

  Test = object
    status: StatusKind
    orig: NimNode
    n: NimNode
    name: string
    number: int

  Styling = distinct string

proc `&`(a, b: Styling): Styling {.borrow.}
proc `&`(a: Styling; b: string): Styling = a & Styling(b)
proc `&`(a: string; b: Styling): Styling = b & a

const
  resetStyle      = Styling ansiResetCode
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
  exceptionStyle  = Styling ansiForegroundColorCode(fgRed, true)
  sourceStyle     = Styling ansiForegroundColorCode(fgDefault)
  viaProcStyle    = Styling ansiStyleCode(styleItalic) &
                    Styling ansiForegroundColorCode(fgBlue, false)
  viaFileStyle    = Styling ansiStyleCode(styleItalic) &
                    Styling ansiStyleCode(styleUnderscore) &
                    Styling ansiForegroundColorCode(fgBlue, true)

proc `$`(style: Styling): string =
  when nimvm:
    # don't try to mess with styling at compile-time
    result = style.string
  else:
    # at runtime, try to emit style if possible
    if stdout.isAtty:
      result = style.string
      if result != resetStyle.string:
        result = resetStyle.string & result

proc `&`(style: Styling; n: NimNode): NimNode =
  let isAtty = bindSym"isAtty"
  var n = newCall(ident"$", n)
  let text = newStmtList(newLit($style), n, newLit($resetStyle))
  result = nnkIfStmt.newNimNode(n)
  result.add nnkElifBranch.newTree(newCall(isAtty, ident"stdout"),
                                   nestList(ident"&", text))
  result.add nnkElse.newTree(n)

var testCount {.compileTime}: int
var testResults = newSeq[int](1 + ord(high StatusKind))

proc incResults(test: Test): NimNode =
  newCall(ident"inc", newTree(nnkBracketExpr,
                              bindSym"testResults",
                              newLit test.status.ord))

template check*(body: typed) =
  if not body:
    dump body
    when (NimMajor, NimMinor) >= (1, 3):
      raise newException(AssertionDefect, "check failed")
    else:
      raise newException(AssertionError, "check failed")

proc `status=`(t: var Test; s: StatusKind) =
  system.`=`(t.status, max(t.status, s))

proc prefixLines(s: string; p: string): string =
  for line in items(splitLines(s, keepEol = true)):
    result.add p & line

proc prefixLines(s: NimNode; p: string): NimNode =
  result = newStmtList()
  for line in items(s):
    result.add infix(p.newLit, "&", line)
  result = nestList(ident"&", result)

proc numberLines(s: string; first = 1): NimNode =
  result = newStmtList()
  for n, line in pairs(splitLines(s, keepEol = true)):
    var ln = lineNumStyle & align($(n + first), 3).newLit
    ln = infix(ln, "&", "  ".newLit)
    ln = infix(ln, "&", sourceStyle & line.newLit)
    result.add ln

proc report(ss: varargs[string, `$`]) =
  writeLine(stderr, ss)

proc output(n: NimNode): NimNode =
  assert not n.isNil
  let report = bindSym"report"
  result = newCall(report, n)

proc output(t: Test; n: NimNode): NimNode =
  assert not n.isNil
  let prefixer = bindSym"prefixLines"
  result = output(newCall(prefixer,
                          newCall(ident"$", n),
                          newLit($t.status & " ")))

proc output(test: Test; styling: Styling; n: NimNode): NimNode =
  assert not n.isNil
  let prefixer = bindSym"prefixLines"
  result = newCall(prefixer, newCall(ident"$", n), newLit($test.status & " "))
  result = test.output(styling & result)

proc success(t: var Test): NimNode =
  t.status = Okay
  result = newStmtList()
  result.add t.incResults
  result.add t.output(successStyle & newLit(t.name))

when false:
  proc countComments(n: NimNode): int =
    assert not n.isNil
    if n.kind == nnkStmtList:
      for _ in items(n):
        if _.kind == nnkCommentStmt:
          result.inc len(_.strVal.splitLines())
        else:
          break

proc fromFileGetLine(file: string; line: int): string =
  let lines = toSeq lines(file)
  result = lines[line - 1]

proc findWhere(s: string; p: string; into: var string): bool =
  result = s.count(p) == 1
  if result:
    into = spaces(s.find(p))

proc renderStack(prefix: string; stack: seq[StackTraceEntry]) =
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
    when false:
      # future substring search
      var where: string
      if findWhere(code, sub, where):
        result.add "$1 $2 ^ (here)" % [ spaces(len(line)), where ]
  report result.join("\n").prefixLines prefix & " 🗇 "

proc renderTrace(t: Test; n: NimNode = nil): NimNode =
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
  result = newIfStmt((prefix(newCall(isAtty, ident"stdout"), "not"),
                      newCall(setResult, code.newLit)))

proc failure(t: var Test; n: NimNode = nil): NimNode =
  t.status = Fail
  result = newStmtList()
  result.add t.incResults
  result.add t.output(failureStyle & newLit(t.name))
  result.add t.renderSource
  result.add t.renderTrace(n)
  result.add t.setExitCode

proc exceptionString(n: NimNode): NimNode =
  assert not n.isNil
  result = infix(infix(newCall(ident"$", newDotExpr(n, ident"name")),
                       "&", ": ".newLit),
                 "&", newDotExpr(n, ident"msg"))

proc badassert(t: var Test; n: NimNode = nil): NimNode =
  ## like failure(), but don't render the stack trace
  t.status = Fail
  result = newStmtList()
  result.add t.incResults
  result.add t.renderSource
  if n.isNil:
    result.add t.output(failureStyle & newLit(t.name))
  else:
    let text = newStmtList(t.name.newLit, newLit(": "),
                           newDotExpr(n, ident"msg"))
    result.add t.output(failureStyle & nestList(ident"&", text))
    result.add t.renderTrace(n)
  result.add t.setExitCode

proc skipped(t: Test; n: NimNode): NimNode =
  assert not n.isNil
  result = newStmtList()
  result.add t.incResults
  result.add t.output(infix(newLit("⚪ " & t.name & ": "), "&",
                            newDotExpr(n, ident"msg")))

proc exception(t: var Test; n: NimNode): NimNode =
  assert not n.isNil
  t.status = Died
  let text = newStmtList(newLit(t.name & ": "), n.exceptionString)
  result = newStmtList()
  result.add t.incResults
  result.add t.output(exceptionStyle & nestList(ident"&", text))
  result.add t.renderSource
  result.add t.renderTrace(n)
  result.add t.setExitCode

proc compilerr(t: var Test): NimNode {.used.} =
  t.status = Oops
  result = newStmtList()
  result.add t.incResults
  result.add t.output(oopsStyle & newLit(t.name & ": compile failed"))
  result.add t.renderSource
  result.add t.setExitCode

proc skip*(msg = "skipped") =
  raise newException(SkipError, msg)

proc wrapExcept(t: var Test): NimNode =
  var skipping = bindSym"SkipError"
  when (NimMajor, NimMinor) >= (1, 3):
    var assertion = bindSym"AssertionDefect"
  else:
    var assertion = bindSym"AssertionError"
  var catchall = bindSym"Exception"
  var e1 {.used.} = genSym(nskLet, "e")
  var e2 {.used.} = genSym(nskLet, "e")
  var e3 {.used.} = genSym(nskLet, "e")
  result = nnkTryStmt.newTree(t.n,
           nnkExceptBranch.newTree(infix(skipping, "as", e1), t.skipped(e1)),
           nnkExceptBranch.newTree(infix(assertion, "as", e2), t.badassert(e2)),
           nnkExceptBranch.newTree(infix(catchall, "as", e3), t.exception(e3)))

proc makeTest(n: NimNode; name: string): Test =
  assert not n.isNil
  result = Test(name: name, orig: n, number: testCount)
  inc testCount
  let beuno {.used.} = genSym(nskLabel, "beuno")  # all good, bro
  let arrrg {.used.} = genSym(nskLabel, "arrrg")  # bad news, pal

  result.n = copyNimTree(n).newStmtList

  if n.kind in testable:
    result.n.add result.success

    # wrap it to catch any exceptions
    result.n = result.wrapExcept
  else:
    # it's not testable; we'll indicate that it worked (what else?)
    when defined(release):
      result.status = Okay
    # output the status in any event; otherwise there will be no output
    result.n.add result.output(newLit result.name)

  # wrap it into `when compiles(original): test else: compilerr`
  when not defined(release):
    result.n = nnkWhenStmt.newTree(
      nnkElifBranch.newTree(
        newCall(ident"compiles",
                nnkBlockStmt.newTree(genSym(nskLabel, "compiles"),
                                     newStmtList(result.orig))),
        result.n),
      nnkElse.newTree(result.compilerr))

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

proc rewriteTestBlock(n: NimNode): NimNode =
  ## rewrite test "something": ... as block: ## something ...
  assert not n.isNil
  result = n
  if n.kind == nnkCommand and len(n) == 3:
    if n[0].kind == nnkIdent and eqIdent(n[0], "test"):
      if n[1].kind == nnkStrLit and n[2].kind == nnkStmtList:
        let name = newCommentStmtNode(n[1].strVal)
        result = nnkBlockStmt.newTree(newEmptyNode(), newStmtList(name, n[2]))

when false:
  proc shortenRepr(n: NimNode): string =
    let splat = repr(n).splitLines
    if len(splat) == 1:
      result = splat[0]
    else:
      result = splat[0] & " ⮠..."

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

proc reportResults(): NimNode =
  var report = bindSym"report"
  var results = bindSym"testResults"
  var join = bindSym"join"
  var lits: seq[NimNode]
  for status in items(StatusKind):
    let brack = nnkBracketExpr.newTree(results, newLit status.ord)
    lits.add newTree(nnkIfStmt,
             newTree(nnkElIfBranch, infix(brack, ">", 0.newLit),
                     infix(newLit $status, "&", newCall(ident"$", brack))),
             newTree(nnkElse, newLit""))
  lits = @[newLit"##"] & lits
  var arr = newTree(nnkBracket, lits)
  result = newCall(report, newCall(join, arr, newLit"  "))

macro testes*(tests: untyped) =
  ## for a good time, put your tests in `block:` underneath the `testes`
  inc depth
  try:
    result = newStmtList()
    for index, n in pairs(tests):
      var n = n.rewriteTestBlock
      var test: Test
      if n.kind == nnkCommentStmt:
        result.add output(commentStyle &
                          newLit(prefixLines(n.strVal, $Info & " ")))
      else:
        let name = findName(n, index)
        test = makeTest(n, name)
        result.add test.n
  finally:
    dec depth
    result.add reportResults()

when isMainModule:
  import std/options

  import bump
  import cligen

  let
    logger = newCuteConsoleLogger()
  addHandler(logger)

  const
    release = projectVersion()
  if release.isSome:
    clCfg.version = $release.get
  else:
    clCfg.version = "(unknown version)"
