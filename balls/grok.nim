import std/macros
import std/strutils

import ./grok/mem
export mem

template crash*(msg = "crash") =
  writeStackTrace()
  debugEcho msg
  quit 1

macro enumValuesAsArray*(e: typed): untyped =
  ## given an enum type, render an array of its symbol fields
  nnkBracket.newNimNode(e).add:
    e.getType[1][1..^1]

macro enumValuesAsSet*(e: typed): untyped =
  ## given an enum type, render a set of its symbol fields
  nnkCurly.newNimNode(e).add:
    e.getType[1][1..^1]

macro enumValuesAsSetOfOrds*(e: typed): untyped =
  ## given an enum type, render a set of its integer values
  result = nnkCurly.newNimNode(e)
  for n in 1 ..< e.getType[1].len:
    result.add:
      newLit e.getType[1][n].intVal

macro newTreeFrom*(kind: NimNodeKind; n: NimNode; body: untyped): NimNode =
  ## use the kind and `n` node to create a new tree;
  ## add the statements in the body and return this node
  var tree = genSym(nskVar, "tree")
  result = newStmtList:
    newVarStmt tree:                           # var tree =
      bindSym"newNimNode".newCall(kind, n)     # newNimNode(kind, n)
  for child in body.items:                     # for child in body:
    add result:
      bindSym"add".newCall tree:               #   add tree:
        child                                  #     child statement
  add result:                                  # tree
    tree

# just a hack to output the example numbers during docgen...
when defined(nimdoc):
  var
    exampleCounter {.compileTime.}: int

when defined(nimdoc):
  import std/strutils

macro ex*(x: untyped): untyped =
  result = x
  when defined(nimdoc):
    for node in x[^1]:
      if node.kind == nnkCall:
        if node[0].kind == nnkIdent:
          if $node[0] == "runnableExamples":
            inc exampleCounter
            let id = repr(x[0])
            hint "fig. $1 for $2:" % [ $exampleCounter, $id ]
            hint indent(repr(node[1]), 4)

proc errorAst*(s: string): NimNode =
  ## produce {.error: s.} in order to embed errors in the ast
  nnkPragma.newTree:
    ident"error".newColonExpr: newLit s

proc errorAst*(n: NimNode; s = "creepy ast"): NimNode =
  ## embed an error with a message
  errorAst s & ":\n" & treeRepr(n) & "\n"

proc accQuoted*(s: string): NimNode = nnkAccQuoted.newTree: ident s

template isNotNil*(x: untyped): bool = not(isNil(x))

macro `//`*(args: varargs[untyped]): untyped =
  ## emit the arguments as a c comment
  var s = @["/*"]
  for item in args.items:
    s.add:
      if item.kind == nnkStrLit:
        item.strVal
      else:
        repr item
  s.add "*/"
  nnkPragma.newTree:
    nnkExprColonExpr.newTree:
      [ident"emit", nnkBracket.newTree s.join(" ").newLit]

when isMainModule:
  type
    TestEnum = enum
      One   = (1, "1st")
      Two   = (2, "2nd")
      Three = (3, "3rd")

  when enumValuesAsArray(TestEnum) != [One, Two, Three]:
    error "enumValuesAsArray is broken"

  when enumValuesAsSet(TestEnum) != {One, Two, Three}:
    error "enumValuesAsSet is broken"
