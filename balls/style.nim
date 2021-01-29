import std/macros
import std/colors

import balls/spec

type
  Styling* = distinct string

proc `&`*(a, b: Styling): Styling {.borrow.}
proc `&`*(a: Styling; b: string): Styling = a & Styling(b)
proc `&`*(a: string; b: Styling): Styling = b & a

{.push hint[ConvFromXtoItselfNotNeeded]: off.}

when defined(js):
  template ansiStyleCode(x: untyped): string = ""
  template ansiForegroundColorCode(x: untyped; bool = true): string = ""
  template ansiBackgroundColorCode(x: untyped; bool = true): string = ""
  const
    ansiResetCode = ""
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
    Pass: successStyle,
    Skip: commentStyle,
    Part: partialStyle,
    Fail: failureStyle,
    Died: exceptionStyle,
    Oops: oopsStyle
  ]

when defined(danger): # avoid unused warnings
  const
    testNumStyle*  = Styling ansiStyleCode(styleItalic) &
                     Styling ansiForegroundColorCode(fgYellow, true)

{.pop.}

proc useColor*(): bool =
  ## for the bland folks; they live among us!
  when ballsDry:
    false
  else:
    when nimvm:
      # don't try to mess with styling at compile-time
      true
    else:
      # at runtime, try to emit style if possible
      when defined(js):
        onCI
      else:
        onCI or stderr.isAtty

proc `$`*(style: Styling): string =
  if useColor():
    result = style.string
    if result != resetStyle.string:
      result = resetStyle.string & result

proc `&`*(style: Styling; n: NimNode): NimNode =
  ## combine style and something $able, but only output the
  ## style if you find that the program is on a tty at runtime
  let useColor = bindSym"useColor"
  let n = dollar n
  let text = newStmtList(newLit($style), n, newLit($resetStyle))
  result = nnkIfStmt.newNimNode(n)
  result.add nnkElifBranch.newTree(newCall(useColor),
                                   nestList(ident"&", text))
  result.add nnkElse.newTree(n)
