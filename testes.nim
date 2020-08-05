import std/os
import std/sequtils
import std/exitprocs
import std/terminal
import std/sugar
import std/strutils
import std/macros

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

template check*(body: typed) =
  if not body:
    dump body
    raise newException(AssertionDefect, "check failed")

proc `status=`(t: var Test; s: StatusKind) =
  system.`=`(t.status, max(t.status, s))

proc prefixLines(s: string; p: string): string =
  for line in items(splitLines(s, keepEol = true)):
    result.add p & line

proc numberLines(s: string): string =
  for n, line in pairs(splitLines(s, keepEol = true)):
    if n > 0:
      result.add "$1  $2" % [ align($n, 3), line ]

proc report(ss: varargs[string, `$`]) =
  writeLine(stderr, ss)

proc output(n: NimNode): NimNode {.deprecated.} =
  assert not n.isNil
  let report = bindSym"report"
  result = newCall(report, n)

proc output(t: Test; n: NimNode): NimNode =
  assert not n.isNil
  let prefixer = bindSym"prefixLines"
  result = output(newCall(prefixer,
                          newCall(ident"$", n),
                          newLit($t.status & " ")))

proc output(t: Test; s: string): NimNode =
  result = t.output s.newLit

proc success(t: var Test): NimNode =
  t.status = Okay
  result = t.output(t.name)

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
      result.add relativePath(cf, path)
    let code = fromFileGetLine(cf, s.line)
    let line = align($s.line, 5)
    result.add "$1 $2  # in $3()" % [ line, code, $s.procname ]
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
  if len(node) > 0:
    if node[0].kind == nnkCommentStmt:
      let dropFirst = node[0].strVal.splitLines(keepEol = true)[1..^1].join("")
      node[0] = newCommentStmtNode(dropFirst)
  result = t.output(repr(node).numberLines.prefixLines " 🗏 ")

proc setExitCode(t: Test; code = QuitFailure): NimNode =
  let isAtty = bindSym"isAtty"
  let setResult = bindSym"setProgramResult"
  result = newIfStmt((prefix(newCall(isAtty, ident"stdin"), "not"),
                      newCall(setResult, code.newLit)))

proc failure(t: var Test; n: NimNode = nil): NimNode =
  t.status = Fail
  result = newStmtList()
  result.add t.output(t.name)
  result.add t.renderSource
  {.warning: "due to nim bug #15160, we have to leave this in!".}
  if not n.isNil:
    result.add t.renderTrace(n)
  result.add t.setExitCode

proc badassert(t: var Test; n: NimNode = nil): NimNode =
  ## like failure(), but don't render the stack trace
  t.status = Fail
  result = newStmtList()
  result.add t.output(t.name)
  result.add t.renderSource
  result.add t.setExitCode

proc exceptionString(n: NimNode): NimNode =
  assert not n.isNil
  result = infix(infix(newCall(ident"$", newDotExpr(n, ident"name")),
                       "&", ": ".newLit),
                 "&", newDotExpr(n, ident"msg"))

proc skipped(t: Test; n: NimNode): NimNode =
  assert not n.isNil
  result = newStmtList()
  result.add t.output(infix(newLit("⚪ " & t.name & ": "), "&",
                            n.exceptionString))

proc exception(t: var Test; n: NimNode): NimNode =
  assert not n.isNil
  t.status = Died
  result = newStmtList()
  result.add t.output(infix(newLit(t.name & ": "), "&",
                            n.exceptionString))
  result.add t.renderSource
  result.add t.renderTrace(n)
  result.add t.setExitCode

proc compilerr(t: var Test): NimNode =
  t.status = Oops
  result = newStmtList()
  result.add t.output(t.name & ": compile failed")
  result.add t.renderSource
  result.add t.setExitCode

proc skip*(msg = "skipped") =
  report "💣 skipping is broken on C (not C++) backend due to a bug"
  raise newException(SkipError, msg)

proc wrapExcept(t: var Test): NimNode =
  var skipping = bindSym"SkipError"
  var assertion = bindSym"AssertionDefect"
  var catchall = bindSym"Exception"
  var e = genSym(nskLet, "e")
  result = nnkTryStmt.newTree(t.n,
           nnkExceptBranch.newTree(infix(skipping, "as", e), t.skipped(e)),
           nnkExceptBranch.newTree(infix(assertion, "as", e), t.failure(e)),
           nnkExceptBranch.newTree(infix(catchall, "as", e), t.exception(e)))

proc makeTest(n: NimNode; name: string): Test =
  assert not n.isNil
  result = Test(name: name, orig: n)
  let beuno = genSym(nskLabel, "beuno")  # all good, bro
  let arrrg = genSym(nskLabel, "arrrg")  # bad news, pal

  result.n = copyNimTree(n).newStmtList

  if n.kind in testable:
    result.n.add result.success

    when false:
      # emit a success if we exited the block normally
      n.add result.success
      n.add nnkBreakStmt.newTree(arrrg)
      result.n = nnkBlockStmt.newTree(beuno, n)

      # emit a failure if we broke out of the success block
      result.n = nnkBlockStmt.newTree(arrrg,
                                      newStmtList(result.n, result.failure))

    # wrap it to catch any exceptions
    result.n = result.wrapExcept
  else:
    # it's not testable; we'll indicate that it worked (what else?)
    when defined(release):
      result.status = Okay
    # output the status in any event; otherwise there will be no output
    result.n.add result.output(result.name)

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

macro testes*(tests: untyped) =
  ## for a good time, put your tests in `block:` underneath the `testes`
  inc depth
  try:
    result = newStmtList()
    for index, n in pairs(tests):
      var n = n.rewriteTestBlock
      var test: Test
      if n.kind == nnkCommentStmt:
        result.add output(newLit(n.strVal.prefixLines($Info & " ")))
      else:
        let name = findName(n, index)
        test = makeTest(n, name)
        result.add test.n
  finally:
    dec depth

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
