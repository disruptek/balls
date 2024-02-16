import std/strutils
import std/times
import std/strformat

const
  ISO8601 = initTimeFormat "yyyy-MM-dd\'T\'HH:mm:ss\'.\'fff\'Z\'"
  ISO8601short = initTimeFormat "yyyy-MM-dd\'T\'HH:mm:ss\'Z\'"

proc pstTzInfo(time: Time): ZonedTime =
  return ZonedTime(utcOffset: 8 * 3600, isDst: false, time: time)

proc pdtTzInfo(time: Time): ZonedTime =
  return ZonedTime(utcOffset: 7 * 3600, isDst: true, time: time)

proc utcTzInfo(time: Time): ZonedTime =
  return ZonedTime(utcOffset: 0 * 3600, isDst: false, time: time)

let tzPST* = newTimezone("Somewhere/PST", pstTzInfo, pstTzInfo)
let tzPDT* = newTimezone("Somewhere/PDT", pdtTzInfo, pdtTzInfo)
let tzUTC* = newTimezone("Somewhere/UTC", utcTzInfo, utcTzInfo)

proc iso8601*(input: DateTime): string =
  ## render input datetime in ISO8601 format
  result = input.format(ISO8601)

proc parseIso8601*(input: string; tz=utc()): DateTime =
  ## parse an input string to ISO8601 datetime
  const shorty = "2019-07-08T22:01:24Z".len
  if input.len == shorty:
    result = parse(input, ISO8601short, tz)
  else:
    result = parse(input, ISO8601, tz)

converter toIso8601*(input: string): DateTime =
  ## auto-convert from string to ISO8601 datetime
  result = input.parseIso8601()

proc shortDuration*(d: Duration): string =
  ## cast a duration to a nice, short string
  let
    n = d.inNanoseconds
  var
    t = {
       "s": (n div 1_000_000_000) mod 1_000,
      "ms": (n div 1_000_000) mod 1_000,
      "μs": (n div 1_000) mod 1_000,
      "ns": (n div 1) mod 1_000,
    }
  for i, n in pairs(t):
    if n[1] > 0:
      return "$1.$2$3" % [ $n[1], $(t[i+1][1] div 100), n[0] ]
  result = "n/a"

proc ft*(d: Duration): string {.raises: [].} =
  ## cast a duration to a nice string
  let
    n = d.inNanoseconds
    ss = (n div 1_000_000_000) mod 1_000
    ms = (n div 1_000_000) mod 1_000
    us = (n div 1_000) mod 1_000
    ns = (n div 1) mod 1_000
  try:
    return fmt"{ss:>3}s {ms:>3}ms {us:>3}μs {ns:>3}ns"
  except:
    return [$ss, $ms, $us, $ns].join(" ")
