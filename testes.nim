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

proc output(n: NimNode): NimNode =
  result = nnkCommand.newTree(ident"echo", n)

proc output(t: Test; n: NimNode): NimNode =
  result = output(n)

proc output(t: Test; s: string): NimNode =
  result = t.output s.newLit

proc success(t: Test): NimNode =
  result = t.output("🟢 " & t.name)

when false:
  proc fromFileGetLine(file: cstring; line: int): string =
    discard

proc renderTrace(s: NimNode): NimNode =
  result = newEmptyNode()

proc renderTrace(t: Test; e: NimNode = nil): NimNode =
  var call = newCall(ident"getStackTrace")
  if not e.isNil:
    call.add e  # get the exception's stacktrace
  result = newIfStmt((newCall(ident"stackTraceAvailable"),
                      renderTrace(call)))

proc numberedLines(s: string): string =
  for n, line in pairs(splitLines(s, keepEol = true)):
    if n > 0:
      result.add "$1  $2" % [ align($n, 3), line ]

proc renderSource(t: Test): NimNode =
  result = t.output(repr(t.orig).numberedLines)

proc setExitCode(t: Test; code = QuitFailure): NimNode =
  let isAtty = bindSym"isAtty"
  let setResult = bindSym"setProgramResult"
  result = newIfStmt((prefix(newCall(isAtty, ident"stdin"), "not"),
                      newCall(setResult, code.newLit)))

proc failure(t: Test): NimNode =
  let renderTrace = bindSym"renderTrace"
  result = newStmtList()
  result.add t.output("🔴 " & t.name)
  result.add t.renderTrace
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
  result.add t.renderTrace(e)
  result.add t.renderSource
  result.add t.setExitCode

proc compilerr(t: Test): NimNode =
  result = newStmtList()
  result.add t.output("⛔ " & t.name & ": compile failed")
  result.add t.renderTrace
  result.add t.renderSource
  result.add t.setExitCode

proc skip*(msg = "skipped") =
  #raise newException(SkipError, msg)
  echo "💣 skipping is broken due to a bug"

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
      result.add output(repr(blok).indent(1).indent(1, "⚫").newLit)
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

template suite*(tests: untyped; title: untyped): untyped =
  ## suite, suite testes
  echo "🔵 " & $title
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
