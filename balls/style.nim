import std/genasts
import std/macros
import std/strutils

import grok

import balls/spec

type
  Styling* = distinct string

proc `&`*(a, b: Styling): Styling {.borrow.}

{.push hint[ConvFromXtoItselfNotNeeded]: off.}

# the idea here is to reduce the amount of ast generated in the event we
# can determine at compile-time that color will/won't be used...

when ballsDry:
  const useColor* = false
  const dynamicColor* = false
elif defined(js) or defined(nimscript):
  const useColor* = false
  const dynamicColor* = false
else:
  import std/terminal
  when onCI:
    # CI is always safe to color
    const useColor* = true
    const dynamicColor* = false
  else:
    when compiles(stdmsg().isAtty):
      let useColor* = stdmsg().isAtty
      const dynamicColor* = true
    else:
      let useColor* = true
      const dynamicColor* = false

when not dynamicColor:
  when not useColor:
    # shim defines from terminal...
    template ansiStyleCode(x: untyped): string {.used.} = ""
    template ansiForegroundColorCode(x: untyped; bool = true): string {.used.} = ""
    template ansiBackgroundColorCode(x: untyped; bool = true): string {.used.} = ""
    const ansiResetCode {.used.} = ""

const
  resetStyle*      = ansiResetCode.Styling
  resultsStyle*    = ansiStyleCode(styleItalic).Styling &
                     ansiForegroundColorCode(fgWhite, true).Styling
  informStyle*     = ansiForegroundColorCode(fgBlue, true).Styling
  commentStyle*    = ansiStyleCode(styleItalic).Styling &
                     ansiForegroundColorCode(fgWhite, true).Styling
  lineNumStyle*    = ansiStyleCode(styleItalic).Styling &
                     ansiForegroundColorCode(fgBlack, true).Styling
  partialStyle*    = ansiForegroundColorCode(fgYellow, true).Styling
  successStyle*    = ansiForegroundColorCode(fgGreen).Styling
  oopsStyle*       = ansiStyleCode(styleBright).Styling &
                     ansiStyleCode(styleReverse).Styling &
                     "\e[48;2;255;255;255m".Styling &  # nim bug
                     ansiForegroundColorCode(fgRed, true).Styling
  failureStyle*    = ansiForegroundColorCode(fgRed).Styling
  skippedStyle*    = ansiStyleCode(styleStrikethrough).Styling &
                     ansiForegroundColorCode(fgMagenta, false).Styling
  exceptionStyle*  = ansiForegroundColorCode(fgRed, true).Styling
  sourceStyle*     = ansiForegroundColorCode(fgDefault).Styling
  viaProcStyle*    = ansiStyleCode(styleItalic).Styling &
                     ansiForegroundColorCode(fgBlue, false).Styling
  viaFileStyle*    = ansiStyleCode(styleItalic).Styling &
                     ansiStyleCode(styleUnderscore).Styling &
                     ansiForegroundColorCode(fgBlue, true).Styling
  headerStyle*     = ansiStyleCode(styleItalic).Styling &
                     ansiStyleCode(styleUnderscore).Styling &
                     ansiForegroundColorCode(fgCyan, true).Styling
  leaderStyle*     = ansiStyleCode(styleItalic).Styling &
                     ansiForegroundColorCode(fgCyan, true).Styling
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
    testNumStyle*  = ansiStyleCode(styleItalic).Styling &
                     ansiForegroundColorCode(fgYellow, true).Styling

{.pop.}

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
  # choose to color at runtime
  template withColor*(a, b, c: untyped): untyped =
    ## first argument is run in the vm,
    ## second argument is run when color is enabled,
    ## third argument is run otherwise.
    when nimvm:
      a
    else:
      if useColor:
        b
      else:
        c

  template withColor*(a, b: untyped): untyped =
    ## second argument is run when color is enabled,
    ## first argument is run otherwise.
    withColor(a, b, a)

  template `$`*(n: Styling): string =
    withColor("", n.string & resetStyle.string, "")
  proc `&`*(style: Styling; n: NimNode): NimNode =
    let colorized =
      nestList ident"&":
        nnkStmtList.newTreeFrom n:
          newLit style.string
          n
          newLit resetStyle.string
    result =
      genAstOpt({}, n, colorized):
        withColor(n, colorized, n)
else:
  when useColor:
    # always color
    template withColor*(a, b, c: untyped): untyped = b
    template `$`*(style: Styling): untyped = style.string
    proc `&`*(style: Styling; n: NimNode): NimNode =
      nestList ident"&":
        nnkStmtList.newTreeFrom n:
          newLit style.string
          n
          newLit resetStyle.string
  else:
    # never any color
    template withColor*(a, b, c: untyped): untyped = c
    template `$`*(style: Styling): string = ""
    proc `&`*(style: Styling; n: NimNode): NimNode = n

macro report*(n: string) =
  ## render a compile-time comment nicely
  var prefix = lineNumStyle.string & "## " & commentStyle.string
  var postfix = newLit resetStyle.string
  let colorized {.used.} = output infix(prefixLines(n, prefix), "&", postfix)
  let bland {.used.} = output prefixLines(n, "## ")
  result =
    genAstOpt({}, bland, colorized):
      withColor(bland, colorized, bland)

proc renderFilename*(filename: string): string =
  ## render a filename nicely
  let path = localPath(filename)
  let bland {.used.} = path
  let colorized {.used.} = "$1$2$3$1" % [ $resetStyle, $viaFileStyle, path ]
  result = withColor(bland, colorized, bland)

proc renderFilename*(s: LineInfo): string =
  renderFilename $s.filename

proc renderLine*(line: int): string =
  ## render a line number nicely
  let bland {.used.} = $line
  let colorized {.used.} = "$1$2$3$1" % [ $resetStyle, $lineNumStyle, $line,
                                          $resetStyle ]
  result = withColor(bland, colorized, bland)

proc renderFilenameAndLine*(s: LineInfo): string =
  ## render a filename and line number nicely
  renderFilename($s.filename) & " " & renderLine(s.line)

proc comment*(n: NimNode): NimNode =
  ## render a comment with the given stringish node
  let bland {.used.} = infix(newLit"## ", "&", n)
  let colorized {.used.} =
    nestList ident"&":
      nnkStmtList.newTreeFrom n:
        newLit lineNumStyle.string & "## "
        n
  result =
    genAstOpt({}, bland, colorized):
      withColor(bland, colorized, bland)
