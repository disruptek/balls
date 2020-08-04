import std/os
import std/sequtils
import std/exitprocs
import std/terminal
import std/unittest except test, skip, check, suite
import std/sugar
import std/strutils
import std/macros

import cutelog

export sugar, strutils, macros, cutelog
export unittest except test, skip, check, suite

type
  SkipError = object of CatchableError
  Test = object
    orig: NimNode
    n: NimNode
    name: string

template check*(body: typed) =
  if not body:
    dump body
    break

proc report(ss: varargs[string, `$`]) =
  writeLine(stderr, ss)

proc output(n: NimNode): NimNode =
  let report = bindSym"report"
  result = newCall(report, n)

proc output(t: Test; n: NimNode): NimNode =
  result = output(n)

proc output(t: Test; s: string): NimNode =
  result = t.output s.newLit

proc success(t: Test): NimNode =
  result = t.output("🟢 " & t.name)

proc countComments(n: NimNode): int =
  if n.kind == nnkStmtList:
    for _ in items(n):
      if _.kind == nnkCommentStmt:
        inc result
      else:
        break

proc prefixLines(s: string; p: string): string =
  for line in items(splitLines(s, keepEol = true)):
    result.add p & line

proc numberLines(s: string): string =
  for n, line in pairs(splitLines(s, keepEol = true)):
    if n > 0:
      result.add "$1  $2" % [ align($n, 3), line ]

proc fromFileGetLine(file: string; line: int): string =
  let lines = toSeq lines(file)
  result = lines[line - 1]

proc renderStack(stack: seq[StackTraceEntry]) =
  var path = getCurrentDir()
  var cf = "////" # an unlikely filename
  var result: seq[string]
  for s in items(stack):
    if $s.filename != cf:
      cf = relativePath($s.filename, path)
      result.add cf
    let code = fromFileGetLine(cf, s.line)
    let line = align($s.line, 5)
    result.add "$1 $2" % [ line, code ]
  report result.join("\n").prefixLines " 🗇 "

proc renderTrace(t: Test; e: NimNode = nil): NimNode =
  var renderStack = bindSym"renderStack"
  var getStack = newCall(ident"getStackTraceEntries")
  if not e.isNil:
    getStack.add e  # get the exception's stacktrace
  result = newIfStmt((newCall(ident"stackTraceAvailable"),
                      newCall(renderStack, getStack)))

proc renderSource(t: Test): NimNode =
  result = t.output(repr(t.orig).numberLines.prefixLines " 🗏 ")

proc setExitCode(t: Test; code = QuitFailure): NimNode =
  let isAtty = bindSym"isAtty"
  let setResult = bindSym"setProgramResult"
  result = newIfStmt((prefix(newCall(isAtty, ident"stdin"), "not"),
                      newCall(setResult, code.newLit)))

proc failure(t: Test): NimNode =
  let renderTrace = bindSym"renderTrace"
  result = newStmtList()
  result.add t.output("🔴 " & t.name)
  result.add t.renderSource
  result.add t.setExitCode

proc exceptionString(e: NimNode): NimNode =
  result = infix(infix(newCall(ident"$", newDotExpr(e, ident"name")),
                       "&", ": ".newLit),
                 "&", newDotExpr(e, ident"msg"))

proc skipped(t: Test; e: NimNode): NimNode =
  result = newStmtList()
  result.add t.output(infix(newLit("⚪ " & t.name & ": "), "&",
                            e.exceptionString))

proc exception(t: Test; e: NimNode): NimNode =
  result = newStmtList()
  result.add t.output(infix(newLit("💥 " & t.name & ": "), "&",
                            e.exceptionString))
  result.add t.renderSource
  result.add t.renderTrace(e)
  result.add t.setExitCode

proc compilerr(t: Test): NimNode =
  result = newStmtList()
  result.add t.output("⛔ " & t.name & ": compile failed")
  result.add t.renderSource
  result.add t.renderTrace
  result.add t.setExitCode

proc skip*(msg = "skipped") =
  #raise newException(SkipError, msg)
  report "💣 skipping is broken due to a bug"

proc wrapExcept(t: Test): NimNode =
  var skip = bindSym"SkipError"
  var e = genSym(nskLet, "e")
  result = nnkTryStmt.newTree(t.n,
           #nnkExceptBranch.newTree(infix(skip, "as", e),
           #                        t.skipped(e)),
           nnkExceptBranch.newTree(infix(ident"Exception", "as", e),
                                   t.exception(e)))

proc makeTest(n: NimNode; name: string): Test =
  var name =
    if len(n) > 0 and n[0].kind == nnkCommentStmt:
      n[0].strVal
    else:
      name
  result = Test(name: name, orig: copyNimTree(n))
  let beuno = genSym(nskLabel, "beuno")  # all good, bro
  let arrrg = genSym(nskLabel, "arrrg")  # bad news, pal

  # emit a success if we exited the block normally
  n.add result.success
  n.add nnkBreakStmt.newTree(arrrg)
  result.n = nnkBlockStmt.newTree(beuno, n)

  # emit a failure if we broke out of the success block
  result.n = nnkBlockStmt.newTree(arrrg,
                                  newStmtList(result.n, result.failure))

  # wrap it to catch any exceptions
  result.n = result.wrapExcept

  # wrap it into `when compiles(original): test else: compilerr`
  result.n = nnkWhenStmt.newTree(
    nnkElifBranch.newTree(
      newCall(ident"compiles", nnkBlockStmt.newTree(newEmptyNode(),
                                                    result.orig)),
      result.n),
    nnkElse.newTree(result.compilerr))

when false:
  proc massageLabel(n: NimNode): NimNode =
    case n.kind
    of nnkStrLit:
      result = genSym(nskLabel, $n.strVal)
    of nnkIntLit:
      result = genSym(nskLabel, $n.intVal)
    else:
      result = n

#template test*(x, y: untyped) = test x: y

proc rewriteTestBlock(n: NimNode): NimNode =
  result = n
  if n.kind == nnkCommand and len(n) == 3:
    if n[0].kind == nnkIdent and eqIdent(n[0], "test"):
      if n[1].kind == nnkStrLit and n[2].kind == nnkStmtList:
        let name = newCommentStmtNode(n[1].strVal)
        result = nnkBlockStmt.newTree(newEmptyNode(), newStmtList(name, n[2]))

macro testes*(tests: untyped) =
  ## for a good time, put your tests in `block:` underneath the `testes`
  result = newStmtList()
  for index, blok in pairs(tests):
    var blok = blok.rewriteTestBlock
    if blok.kind != nnkBlockStmt:
      result.add output(repr(blok).prefixLines("⚫ ").newLit)
      result.add blok
    else:
      var test: Test
      if len(blok) < 2 or len(blok.last) == 0:
        test.name = "test #$1 omitted" % [ $index ]
        test.n = test.output("🔵 " & test.name)
      elif len(blok) == 2 and blok[0].kind == nnkEmpty:
        test = makeTest(blok.last, "test #" & $index)
      elif len(blok) == 2 and blok[0].kind != nnkEmpty:
        test = makeTest(blok.last, blok[0].strVal.replace("_", " "))
      else:
        raise newException(Defect, "unsupported")
      result.add test.n

template suite*(title, tests: untyped): untyped =
  ## suite, suite testes
  report "🔵 " & $title
  result = testes: tests

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
