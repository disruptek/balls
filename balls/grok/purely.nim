import std/macros

when (NimMajor, NimMinor) < (1, 2):
  {.error: "purely requires nim-1.2+".}

type
  NodeFilter = proc(n: NimNode): NimNode

proc filter(n: NimNode; f: NodeFilter): NimNode =
  result = f(n)
  if result.isNil:
    result = copyNimNode n
    for kid in items(n):
      result.add filter(kid, f)

proc purelyImpl(n: NimNode; prefix: string): NimNode =
  n.expectKind nnkTypeDef
  result = n
  if n.len > 0 and n[0].kind == nnkPragmaExpr:
    if n[^1].kind == nnkEnumTy:
      proc purify(n: NimNode): NimNode =
        if n.kind == nnkEnumTy:
          for pos in 1 ..< n.len:
            if n[pos].kind in {nnkIdent}:
              n[pos] = ident(prefix & n[pos].strVal)
      n[^1] = filter(n.last, purify)

macro purely*(s: string; n: untyped{nkTypeDef}): untyped =
  result = purelyImpl(n, s.strVal)

when isMainModule:
  type
    Yep {.purely: "foo".} = enum
      One
      Two

  discard fooOne
