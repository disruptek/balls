import std/strutils

import balls/style

type
  Tabouli* = object
    headers*: seq[string]
    rows*: seq[seq[string]]
    freeze*: int              # column freeze, like in a spreadsheet

proc render*(t: Tabouli): string =
  ## render a table as a string, perhaps with style
  const
    pad = "  "
  result = $headerStyle
  var widths = newSeq[int](len t.headers)

  # calculate the widths, and...
  for i, s in t.headers.pairs:
    widths[i] = len s
    for row in t.rows.items:
      if i < t.freeze:
        widths[i] = max(widths[i], len row[i])
      else:
        break
    # ...add the headers such that they begin/end at the corners
    if i == 0:
      result.add alignLeft(s, widths[i])  # top/left corner
    else:
      result.add align(s, widths[i])      # top/right corner
    if i == widths.high:
      result.add $resetStyle    # reset the style at the end
    else:
      result.add pad            # space the columns

  # now we simply add the rows
  for r, row in t.rows.pairs:
    result.add "\n"
    result.add $leaderStyle
    for i, s in row.pairs:
      if i < t.freeze:
        # right-align the early columns
        result.add align(s, widths[i])
      else:
        # NOTE: later columns are aligned, but we don't use align()
        # 'cause it won't understand our embedded style controls
        result.add spaces(widths[i] - 1)
        result.add s
      if i == 0:
        result.add $resetStyle    # reset the style after the leader
      if i == widths.high:
        result.add $resetStyle    # reset the style at the end
      else:
        result.add pad            # space the columns
