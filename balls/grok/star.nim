import std/macros

type
  NodeFilter = proc(n: NimNode): NimNode

proc filter(n: NimNode; f: NodeFilter): NimNode =
  result = f(n)
  if result.isNil:
    result = copyNimNode n
    for kid in items(n):
      result.add filter(kid, f)

proc exportify(n: NimNode): NimNode =
  if n.kind == nnkIdentDefs:
    for pos in 0 .. n.len - 3:
      if n[pos].kind in {nnkSym, nnkIdent}:
        n[pos] = postfix(n[pos], "*")

proc starImpl(n: NimNode; force = false): NimNode =
  n.expectKind nnkTypeDef # ^ duh
  result = n
  if n.len > 0 and n[0].kind == nnkPragmaExpr:
    let pm = n[0]
    if pm.len > 0:
      var force = force
      if pm[0].kind == nnkPostfix:
        let post = pm[0]
        if post.len > 0 and eqIdent(post[0], ident"*"):
          if n.last.kind == nnkObjectTy:
            force = true
      elif force:
        pm[0] = postfix(pm[0], "*")
      if force:
        n[^1] = filter(n.last, exportify)

macro star*(n: untyped{nkTypeDef}): untyped =
  result = starImpl n

macro starIf*(b: bool; n: untyped{nkTypeDef}): untyped =
  if b.boolVal:
    n.starImpl true
  else:
    n

when isMainModule:
  macro hasStar(n: typed, op: string): bool =
    var n = n
    if n.kind == nnkCheckedFieldExpr:
      n = n[0]
    result = infix(newLit n[0].getTypeInst.isExported,
                   op.strVal, newLit n[1].isExported)

  type
    Yep* {.star.} = object
      one: int
      two: float
      x, y: string
    Nah {.star.} = object
      one: int
      two: float
    Oof* {.star.} = object
      case kind: bool
      of on:
        one: int
      of off:
        two: float
      x, y: string
    Dev {.starIf: not defined(release).} = object
      one: int

  let a = Yep()
  let b = Nah()
  let c = Oof(kind: on)
  let d = Dev()

  assert a.one.hasStar("and")
  assert a.two.hasStar("and")
  assert a.y.hasStar("and")
  assert not b.one.hasStar("or")
  assert not b.two.hasStar("or")
  assert c.kind.hasStar("and")
  assert c.one.hasStar("and")
  when defined(release):
    assert not d.one.hasStar("or")
  else:
    assert d.one.hasStar("and")
