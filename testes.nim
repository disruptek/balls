import std/strutils
import std/macros

import cutelog

proc output(n: NimNode): NimNode =
  result = nnkCommand.newTree(ident"echo", n)

proc output(s: string): NimNode =
  result = output s.newLit

proc success(name: string): NimNode =
  result = output("🟢 " & name)

proc failure(name: string): NimNode =
  result = output("🔴 " & name)

proc exception(e: NimNode; name: string): NimNode =
  result = output(infix(newLit("💥 " & name & ": "), "&",
                        newDotExpr(e, ident"msg")))

proc wrapExcept(n: NimNode; name: string): NimNode =
  var uhoh = newStmtList()
  var e = genSym(nskLet, "e")
  result = nnkTryStmt.newTree(n,
           nnkExceptBranch.newTree(infix(ident"Exception", "as", e),
                                   e.exception(name)))

proc makeTest(n: NimNode; name: string): NimNode =
  var name =
    if n[0].kind == nnkCommentStmt:
      n[0].strVal
    else:
      name
  let beuno = genSym(nskLabel, "beuno")
  let arrrg = genSym(nskLabel, "arrrg")
  n.add success(name)
  n.add nnkBreakStmt.newTree(arrrg)
  result = nnkBlockStmt.newTree(beuno, n)
  result = nnkBlockStmt.newTree(arrrg, newStmtList(result, failure(name)))
  result = result.wrapExcept(name)

proc massageLabel(n: NimNode): NimNode =
  case n.kind
  of nnkStrLit:
    result = genSym(nskLabel, $n.strVal)
  of nnkIntLit:
    result = genSym(nskLabel, $n.intVal)
  else:
    result = n

macro testes*(tests: untyped) =
  echo treeRepr(tests)
  result = newStmtList()
  for index, blok in pairs(tests):
    if blok.kind != nnkBlockStmt:
      result.add blok
    else:
      if len(blok) == 2 and blok[0].kind != nnkEmpty:
        result.add blok.last.makeTest(blok[0].strVal.replace("_", " "))
      elif len(blok) == 1:
        result.add blok.last.makeTest("block " & $index)
      else:
        warning "ignoring empty block"

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
