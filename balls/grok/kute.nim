import std/math
import std/strformat

type
  Kute* = distinct int64
  KuteUnit* = enum
    Bytes      = "b"
    KiloBytes  = "kb"
    MegaBytes  = "mb"
    GigaBytes  = "gb"
    TeraBytes  = "tb"
    PetaBytes  = "pb"
    ExaBytes   = "eb"
    ZettaBytes = "zb"
    YottaBytes = "yb"

const
  k = 1024
  size = len("1023.9" & $KuteUnit.high)

proc `<`*(a, b: Kute): bool {.borrow.}
proc `==`*(a, b: Kute): bool {.borrow.}
proc `div`*(a, b: Kute): int64 {.borrow.}
proc `mod`*(a, b: Kute): int64 {.borrow.}
converter toBase*(b: Kute): int64 = int64 b

converter `$`*(b: Kute): string =
  result = newStringOfCap size
  var i = 0
  # start low, go high
  while i <= KuteUnit.high.int:
    # r is the next largest unit
    let r = k ^ (i+1)
    if b > r:
      # go to the next unit
      inc i
    else:
      if i == 0:
        # the first unit gets special treatment
        result.add system.`$`(b)
      else:
        # else, gimme the decimal remainder
        let r = k ^ i
        let bdr = b div r
        if b mod r > 0 and bdr < 10:
          result.add fmt"{math.round(b.float / r.float, 1):>01}"
        else:
          result.add $bdr
      result.add system.`$`(KuteUnit i)
      break
  assert result.len <= size  # make sure we don't alloc somehow

when isMainModule:
  import balls

  suite "kute":
    block:
      ## string rendering
      check "unexpected string rendering":
        $Kute(812) == "812b"
        $Kute(8192) == "8kb"
        $Kute(8900) == "8.7kb"
        $Kute(10*8500) == "83kb"
        $Kute(100*8500) == "830kb"
        $Kute(1000*8500) == "8.1mb"

    block:
      ## string conversion
      check "unexpected string conversion":
        Kute(812) == "812b"
        Kute(8192) == "8kb"
        Kute(8900) == "8.7kb"
        Kute(10*8500) == "83kb"
        Kute(100*8500) == "830kb"
        Kute(1000*8500) == "8.1mb"
        Kute(10000*8500) == "81mb"
