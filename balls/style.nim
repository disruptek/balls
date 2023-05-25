import std/colors
import std/macros
import std/strutils

import grok

import balls/spec

type
  Styling* = distinct string

proc `&`*(a, b: Styling): Styling {.borrow.}
proc `&`*(a: Styling; b: string): Styling = a & Styling(b)
proc `&`*(a: string; b: Styling): Styling = b & a

{.push hint[ConvFromXtoItselfNotNeeded]: off.}

when defined(js) or defined(nimscript):
  template ansiStyleCode(x: untyped): string = ""
  template ansiForegroundColorCode(x: untyped; bool = true): string = ""
  template ansiBackgroundColorCode(x: untyped; bool = true): string = ""
  const ansiResetCode = ""
else:
  import std/terminal

const
  resetStyle*      = Styling ansiResetCode
  resultsStyle*    = Styling ansiStyleCode(styleItalic) &
                     Styling ansiForegroundColorCode(fgWhite, true)
  informStyle*     = Styling ansiForegroundColorCode(fgBlue, true)
  commentStyle*    = Styling ansiStyleCode(styleItalic) &
                     Styling ansiForegroundColorCode(fgWhite, true)
  lineNumStyle*    = Styling ansiStyleCode(styleItalic) &
                     Styling ansiForegroundColorCode(fgBlack, true)
  partialStyle*    = Styling ansiForegroundColorCode(fgYellow, true)
  successStyle*    = Styling ansiForegroundColorCode(fgGreen)
  oopsStyle*       = Styling ansiStyleCode(styleBright) &
                     Styling ansiStyleCode(styleReverse) &
                     Styling ansiBackgroundColorCode(Color(0xFFFFFF)) &
                     Styling ansiForegroundColorCode(fgRed, true)
  failureStyle*    = Styling ansiForegroundColorCode(fgRed)
  skippedStyle*    = Styling ansiStyleCode(styleStrikethrough) &
                     Styling ansiForegroundColorCode(fgMagenta, false)
  exceptionStyle*  = Styling ansiForegroundColorCode(fgRed, true)
  sourceStyle*     = Styling ansiForegroundColorCode(fgDefault)
  viaProcStyle*    = Styling ansiStyleCode(styleItalic) &
                     Styling ansiForegroundColorCode(fgBlue, false)
  viaFileStyle*    = Styling ansiStyleCode(styleItalic) &
                     Styling ansiStyleCode(styleUnderscore) &
                     Styling ansiForegroundColorCode(fgBlue, true)
  headerStyle*     = Styling ansiStyleCode(styleItalic) &
                     Styling ansiStyleCode(styleUnderscore) &
                     Styling ansiForegroundColorCode(fgCyan, true)
  leaderStyle*     = Styling ansiStyleCode(styleItalic) &
                     Styling ansiForegroundColorCode(fgCyan, true)
  statusStyles*: array[StatusKind, Styling] = [
    None: resetStyle,
    Info: informStyle,
    Wait: informStyle,
    Runs: informStyle,
    Pass: successStyle,
    Skip: commentStyle,
    Part: partialStyle,
    Fail: failureStyle,
    Died: exceptionStyle,
    Oops: oopsStyle
  ]

when ballsAuditTimeSpace: # avoid unused warnings
  const
    testNumStyle*  = Styling ansiStyleCode(styleItalic) &
                     Styling ansiForegroundColorCode(fgYellow, true)

{.pop.}

#when nimvm:
#  # don't try to mess with styling at compile-time
#  template useColor* = false
#  const dynamicColor = true
#else:
when ballsDry:
  const useColor* = false
  const dynamicColor* = false
  const hasColor = false
elif defined(js) or defined(nimscript):
  # don't bother with these yet
  const useColor* = false
  const dynamicColor* = false
  const hasColor = false
elif onCI:
  # CI is always safe to color
  const useColor* = true
  const dynamicColor* = false
  const hasColor = true
else:
  # else, color only if we're on a tty
  let hasColor = stdmsg().isAtty
  #const hasColor = true
  macro useColor*: untyped = bindSym"hasColor"
  const dynamicColor* = true

proc checkpoint*(ss: varargs[string, `$`]) =
  ## Like `echo`, but outputs to `stdmsg()` with the other test output.
  noclobber:
    when defined(js) or defined(nimscript):
      echo ss.join(" ")
    else:
      stdmsg().writeLine ss.join(" ")

proc output*(n: NimNode): NimNode =
  assert not n.isNil
  result = newCall(bindSym"checkpoint", n)

proc prefixLines*(s: NimNode; p: string): NimNode =
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

when dynamicColor:
  proc `$`*(style: Styling): string =
    result = style.string
    if hasColor:
      if not result.startsWith(resetStyle.string):
        result = resetStyle.string & result

  proc `&`*(style: Styling; n: NimNode): NimNode =
    ## combine style and something $able, but only output the
    ## style if you find that the program is on a tty at runtime
    if style.string == "":
      n # this dollar is sometimes gratuitous...  FIXME: wut.
    else:
      nnkIfStmt.newTreeFrom n:
        nnkElifBranch.newTreeFrom n:
          bindSym"hasColor"
          nestList ident"&":
            nnkStmtList.newTreeFrom n:
              newLit style.string
              n
              newLit resetStyle.string
        nnkElse.newTree:
          n

  macro report*(n: string) =
    ## render a multi-line comment
    var prefix = $lineNumStyle & "## " & $commentStyle
    var postfix = newLit resetStyle.string
    result =
      nnkIfStmt.newTreeFrom n:
        nnkElifExpr.newTreeFrom n:
          bindSym"useColor"
          output infix(prefixLines(n, prefix), "&", postfix)
        nnkElse.newTree:
          output prefixLines(n, "## ")

else:
  when useColor:
    proc `$`*(style: Styling): string =
      result = style.string
      if not result.startsWith(resetStyle.string):
        result = resetStyle.string & result
    proc `&`*(style: Styling; n: NimNode): NimNode =
      nestList ident"&":
        nnkStmtList.newTreeFrom n:
          newLit style.string
          dollar n
          newLit resetStyle.string
      dollar n
  else:
    template `$`*(style: Styling): string = ""
    proc `&`*(style: Styling; n: NimNode): NimNode = n
    macro report*(n: string) = output prefixLines(n, "## ")
