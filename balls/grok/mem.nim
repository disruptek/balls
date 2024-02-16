proc quiesceMemory*(message: string): int {.inline.} =
  when not defined(nimscript):
    when not defined(js):
      when not defined(gcArc) and not defined(gcOrc):
        GC_fullCollect()
    when defined(debugMemory):
      stdmsg().writeLine GC_getStatistics()
    result = getOccupiedMem()

template dumpMem*() =
  when defined(debugMemory):
    when defined(nimTypeNames):
      dumpNumberOfInstances()
    stdmsg().writeLine "total: " & $getTotalMem()
    stdmsg().writeLine " free: " & $getFreeMem()
    stdmsg().writeLine "owned: " & $getOccupiedMem()
    stdmsg().writeLine "  max: " & $getMaxMem()
