import std/strutils
import std/times
import std/strformat

proc shortDuration*(d: Duration): string =
  ## cast a duration to a nice, short string
  let n = d.inNanoseconds
  if n == 0:
    return "n/a"
  var t = {
     "s": (n div 1_000_000_000) mod 1_000,
    "ms": (n div 1_000_000) mod 1_000,
    "μs": (n div 100_000) mod 1_000,
    "ns": (n div 1) mod 1_000,
  }
  for i, n in pairs(t):
    if n[1] > 0:
      case i
      of 3: return "$1$2" % [ $n[1], n[0] ]
      else: return "$1.$2$3" % [ $n[1], $(t[i+1][1] div 100), n[0] ]

proc ft*(d: Duration): string {.raises: [].} =
  ## cast a duration to a nice string
  let
    n = d.inNanoseconds
    ss = (n div 1_000_000_000) mod 1_000
    ms = (n div 1_000_000) mod 1_000
    us = (n div 1_000_000) mod 1_000
    ns = (n div 1) mod 1_000
  try:
    return fmt"{ss:>3}s {ms:>3}ms {us:>3}μs {ns:>3}ns"
  except:
    return [$ss, $ms, $us, $ns].join(" ")
