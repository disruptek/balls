import std/posix

# macOS private API and kqueue based signalfd emulation
when defined(macosx) or defined(osx) or defined(darwin):
  const
    EVFILT_SIGNAL* = -6
    EV_ADD* = 0x0001
    EV_ENABLE* = 0x0004
    EV_CLEAR* = 0x0020
    SFD_NONBLOCK* = 0x800
    SFD_CLOEXEC* = 0x80000

  type
    KEvent* {.importc: "struct kevent", header: "<sys/event.h>".} = object
      ident*: uint
      filter*: int16
      flags*: uint16
      fflags*: uint32
      data*: int
      udata*: pointer

    signalfd_siginfo* = object
      ssi_signo*: uint32
      ssi_errno*: int32
      ssi_code*: int32
      ssi_pid*: uint32
      ssi_uid*: uint32
      ssi_fd*: int32
      ssi_tid*: uint32
      ssi_band*: uint32
      ssi_overrun*: uint32
      ssi_trapno*: uint32
      ssi_status*: int32
      ssi_int*: int32
      ssi_ptr*: uint64
      ssi_utime*: uint64
      ssi_stime*: uint64
      ssi_addr*: uint64
      ssi_addr_lsb*: uint16
      pad2: uint16
      ssi_syscall*: uint32
      ssi_call_addr*: uint64
      ssi_arch*: uint32
      pad: array[0..27, uint8]

  proc kqueue*(): cint {.importc: "kqueue", header: "<sys/event.h>".}
  proc kevent*(kq: cint, changelist: ptr KEvent, nchanges: cint,
               eventlist: ptr KEvent, nevents: cint,
               timeout: ptr Timespec): cint {.importc: "kevent", header: "<sys/event.h>".}

  # ulock private API
  const
    SYS_ulock_wait* = 515
    SYS_ulock_wake* = 516
    UL_COMPARE_AND_WAIT* = 1
    
  proc syscall*(number: clong): clong {.importc: "syscall", header: "<unistd.h>", varargs.}

  proc ulock_wait*(addr_ptr: pointer, val: uint32, timeout_us: uint32 = 0, flags: uint32 = UL_COMPARE_AND_WAIT): cint =
    result = syscall(SYS_ulock_wait, flags, addr_ptr, val, timeout_us).cint

  proc ulock_wake*(addr_ptr: pointer, flags: uint32 = UL_COMPARE_AND_WAIT): cint =
    result = syscall(SYS_ulock_wake, flags, addr_ptr, 0).cint

  type Fd* = cint

  proc signalfd*(fd: Fd, mask: ptr Sigset, flags: cint): Fd =
    let kq = if fd == -1: kqueue() else: fd
    if kq == -1: return -1
    result = kq

  proc readSigInfo*(fd: Fd, info: var signalfd_siginfo): bool =
    var ev: KEvent
    var ts: Timespec
    let n = kevent(fd, nil, 0, addr ev, 1, addr ts)
    if n > 0 and ev.filter == EVFILT_SIGNAL:
      info.ssi_signo = ev.ident.uint32
      return true
    return false
