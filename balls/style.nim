import std/genasts
import std/macros
import std/strutils

import balls/grok
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

# Pre-compute colored versions as constants (only concatenated once)
const
  coloredResetStyle*      = ansiResetCode
  coloredResultsStyle*    = ansiStyleCode(styleItalic) & ansiForegroundColorCode(fgWhite, true)
  coloredInformStyle*     = ansiForegroundColorCode(fgBlue, true)
  coloredCommentStyle*    = ansiStyleCode(styleItalic) & ansiForegroundColorCode(fgWhite, true)
  coloredLineNumStyle*    = ansiStyleCode(styleItalic) & ansiForegroundColorCode(fgBlack, true)
  coloredPartialStyle*    = ansiForegroundColorCode(fgYellow, true)
  coloredSuccessStyle*    = ansiForegroundColorCode(fgGreen)
  coloredOopsStyle*       = ansiStyleCode(styleBright) & ansiStyleCode(styleReverse) & "\e[48;2;255;255;255m" & ansiForegroundColorCode(fgRed, true)
  coloredFailureStyle*    = ansiForegroundColorCode(fgRed)
  coloredSkippedStyle*    = ansiStyleCode(styleStrikethrough) & ansiForegroundColorCode(fgMagenta, false)
  coloredExceptionStyle*  = ansiForegroundColorCode(fgRed, true)
  coloredSourceStyle*     = ansiForegroundColorCode(fgDefault)
  coloredViaProcStyle*    = ansiStyleCode(styleItalic) & ansiForegroundColorCode(fgBlue, false)
  coloredViaFileStyle*    = ansiStyleCode(styleItalic) & ansiStyleCode(styleUnderscore) & ansiForegroundColorCode(fgBlue, true)
  coloredHeaderStyle*     = ansiStyleCode(styleItalic) & ansiStyleCode(styleUnderscore) & ansiForegroundColorCode(fgCyan, true)
  coloredLeaderStyle*     = ansiStyleCode(styleItalic) & ansiForegroundColorCode(fgCyan, true)

when ballsAuditTimeSpace:
  const
    coloredTestNumStyle*  = ansiStyleCode(styleItalic) & ansiForegroundColorCode(fgYellow, true)

# For static (non-dynamic) color modes, select at compile time
when not dynamicColor:
  const
    resetStyle*      = (if useColor: coloredResetStyle else: "").Styling
    resultsStyle*    = (if useColor: coloredResultsStyle else: "").Styling
    informStyle*     = (if useColor: coloredInformStyle else: "").Styling
    commentStyle*    = (if useColor: coloredCommentStyle else: "").Styling
    lineNumStyle*    = (if useColor: coloredLineNumStyle else: "").Styling
    partialStyle*    = (if useColor: coloredPartialStyle else: "").Styling
    successStyle*    = (if useColor: coloredSuccessStyle else: "").Styling
    oopsStyle*       = (if useColor: coloredOopsStyle else: "").Styling
    failureStyle*    = (if useColor: coloredFailureStyle else: "").Styling
    skippedStyle*    = (if useColor: coloredSkippedStyle else: "").Styling
    exceptionStyle*  = (if useColor: coloredExceptionStyle else: "").Styling
    sourceStyle*     = (if useColor: coloredSourceStyle else: "").Styling
    viaProcStyle*    = (if useColor: coloredViaProcStyle else: "").Styling
    viaFileStyle*    = (if useColor: coloredViaFileStyle else: "").Styling
    headerStyle*     = (if useColor: coloredHeaderStyle else: "").Styling
    leaderStyle*     = (if useColor: coloredLeaderStyle else: "").Styling
  
  const
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
  
  when ballsAuditTimeSpace:
    const
      testNumStyle* = (if useColor: coloredTestNumStyle else: "").Styling
else:
  # For dynamic color mode, select at runtime
  let
    resetStyle*      {.used.} = (if useColor: coloredResetStyle else: "").Styling
    resultsStyle*    {.used.} = (if useColor: coloredResultsStyle else: "").Styling
    informStyle*     {.used.} = (if useColor: coloredInformStyle else: "").Styling
    commentStyle*    {.used.} = (if useColor: coloredCommentStyle else: "").Styling
    lineNumStyle*    {.used.} = (if useColor: coloredLineNumStyle else: "").Styling
    partialStyle*    {.used.} = (if useColor: coloredPartialStyle else: "").Styling
    successStyle*    {.used.} = (if useColor: coloredSuccessStyle else: "").Styling
    oopsStyle*       {.used.} = (if useColor: coloredOopsStyle else: "").Styling
    failureStyle*    {.used.} = (if useColor: coloredFailureStyle else: "").Styling
    skippedStyle*    {.used.} = (if useColor: coloredSkippedStyle else: "").Styling
    exceptionStyle*  {.used.} = (if useColor: coloredExceptionStyle else: "").Styling
    sourceStyle*     {.used.} = (if useColor: coloredSourceStyle else: "").Styling
    viaProcStyle*    {.used.} = (if useColor: coloredViaProcStyle else: "").Styling
    viaFileStyle*    {.used.} = (if useColor: coloredViaFileStyle else: "").Styling
    headerStyle*     {.used.} = (if useColor: coloredHeaderStyle else: "").Styling
    leaderStyle*     {.used.} = (if useColor: coloredLeaderStyle else: "").Styling
  
  var
    statusStyles* {.used.}: array[StatusKind, Styling] = [
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
  
  when ballsAuditTimeSpace:
    let
      testNumStyle* {.used.} = (if useColor: coloredTestNumStyle else: "").Styling

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
  result = newStmtList()
  for line in ss.items:
    result.add infix(p.newLit, "&", line)
  # Nest the & operations to create proper concatenation
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
          newLit coloredResetStyle
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
          newLit resetStyle
  else:
    # never any color
    template withColor*(a, b, c: untyped): untyped = c
    template `$`*(style: Styling): string = ""
    proc `&`*(style: Styling; n: NimNode): NimNode = n

macro report*(n: string) =
  ## render a compile-time comment nicely
  let coloredPrefix = coloredLineNumStyle & "## " & coloredCommentStyle
  let plainPrefix = "## "
  let coloredPostfix = coloredResetStyle
  let colorized {.used.} = output infix(prefixLines(n, coloredPrefix), "&", newLit coloredPostfix)
  let bland {.used.} = output prefixLines(n, plainPrefix)
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
  let coloredPrefix = coloredLineNumStyle & "## "
  let colorized {.used.} =
    infix(infix(newLit coloredPrefix, "&", n), "&", newLit coloredResetStyle)
  result =
    genAstOpt({}, bland, colorized):
      withColor(bland, colorized, bland)
