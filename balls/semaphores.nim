import std/hashes
import std/locks

type
  Semaphore* = object
    lock: Lock
    cond: Cond
    count: int

proc hash*(s: var Semaphore): Hash =
  ## whatfer inclusion in a table, etc.
  hash(cast[int](addr s))

proc initSemaphore*(s: var Semaphore; count: int = 0) =
  ## make a semaphore available for use
  initLock s.lock
  initCond s.cond
  s.count = count

proc `=destroy`*(s: var Semaphore) =
  deinitLock s.lock
  deinitCond s.cond
  s.count = 0

proc `=copy`*(s: var Semaphore; e: Semaphore)
  {.error: "semaphores cannot be copied".} =
  discard

proc acquire*(s: var Semaphore) =
  ## adhoc acquire of semaphore's lock
  acquire s.lock

proc release*(s: var Semaphore) =
  ## adhoc release of semaphore's lock
  release s.lock

template withLock*(s: var Semaphore; logic: untyped) =
  ## run the `logic` while holding the semaphore `s`'s lock
  acquire s
  try:
    logic
  finally:
    release s

proc signal*(s: var Semaphore) =
  ## blocking signal of `s`; increments semaphore
  withLock s.lock:
    inc s.count
    signal s.cond

proc wait*(s: var Semaphore) =
  ## blocking wait on `s`
  while true:
    acquire s.lock
    if s.count > 0:
      dec s.count
      release s.lock
      break
    wait(s.cond, s.lock)
    release s.lock

proc available*(s: var Semaphore): int =
  ## blocking count of `s`
  withLock s.lock:
    result = s.count

template isReady*(s: var Semaphore): untyped =
  ## blocking `true` if `s` is ready
  s.available > 0

proc inc*(s: var Semaphore) =
  ## blocking adhoc adjustment of the semaphore
  withLock s.lock:
    inc s.count

proc dec*(s: var Semaphore) =
  ## blocking adhoc adjustment of the semaphore
  withLock s.lock:
    dec s.count

template withSemaphore*(s: var Semaphore; logic: typed): untyped =
  ## wait for the semaphore `s`, run the `logic`, and signal it
  wait s
  try:
    logic
  finally:
    signal s
