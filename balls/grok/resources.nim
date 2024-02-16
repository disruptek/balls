when not (defined(linux) or defined(macosx) or defined(bsd) or defined(solaris) or defined(aix)):
  import std/macros
  {.error: "get a real os".}
else:
  import std/posix

  type
    RUsagePtr = ptr RUsage
    RUsageKind = static[cint]
    Resources[T: RUsageKind] = distinct RUsage
    ProcessResources* = Resources[RUsageSelf] ##
      ## a JIT-sampled distinct of posix.Rusage run against the process
    ThreadResources* = Resources[RUsageThread] ##
      ## a JIT-sampled distinct of posix.Rusage run against the thread
    ChildResources* = Resources[RUsageChildren] ##
      ## a JIT-sampled distinct of posix.Rusage run for all children

  template isInitialized*(ru: Resources): untyped =
    ## true if the resources have been sampled
    Rusage(ru).ru_maxrss != 0

  proc sample*[who: static[cint]](ru: var Resources[who]) =
    ## retrieve resource counters from the kernel
    if 0 != getrusage(who, RusagePtr(addr ru)):
      raise OSError.newException $errno & ":" & $strerror(errno)

  template initialize[who: static[cint]](ru: var Resources[who]): untyped =
    if not ru.isInitialized:
      sample ru

  template withSampled(ru: var Resources; logic: untyped): untyped =
    initialize ru
    logic

  template withSampled(ru: Resources; logic: untyped): untyped =
    if ru.isInitialized:
      logic
    else:
      const message = $typeof(ru) & " is not initialized"
      raise ValueError.newException message

  proc maxResidentBytes*(ru: var Resources): int {.inline.} =
    ## retrieve the maximum resident set size in bytes
    withSampled ru:
      Rusage(ru).ru_maxrss.int * 1024

  proc maxResidentBytes*(ru: Resources): int {.inline.} =
    ## retrieve the maximum resident set size in bytes
    withSampled ru:
      Rusage(ru).ru_maxrss.int * 1024

  proc voluntaryContextSwitches*(ru: var Resources): int {.inline.} =
    ## retrieve the number of voluntary context switches
    withSampled ru:
      Rusage(ru).ru_nvcsw.int

  proc voluntaryContextSwitches*(ru: Resources): int {.inline.} =
    ## retrieve the number of voluntary context switches
    withSampled ru:
      Rusage(ru).ru_nvcsw.int

  proc involuntaryContextSwitches*(ru: var Resources): int {.inline.} =
    ## retrieve the number of involuntary context switches
    withSampled ru:
      Rusage(ru).ru_nivcsw.int

  proc involuntaryContextSwitches*(ru: Resources): int {.inline.} =
    ## retrieve the number of involuntary context switches
    withSampled ru:
      Rusage(ru).ru_nivcsw.int
