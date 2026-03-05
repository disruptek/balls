# macOS Native Support (Native & Private APIs)

This document outlines the architectural decisions and implementation details for supporting `balls` natively on macOS (Darwin).

## Context

The `balls` test runner originally depended on `insideout`, which relies heavily on Linux-specific features such as `sys/signalfd.h`, `epoll`, and `futex` syscalls. To enable `balls` to run natively and efficiently on macOS, these dependencies had to be replaced with Darwin-native alternatives.

Following the design pattern established in `insideout` (PR #30), we have opted to use a combination of public BSD APIs and macOS-specific private APIs.

## Implementation Details

### 1. Signal Handling (Replacement for `signalfd`)

MacOS does not have an equivalent to Linux's `signalfd`. To provide similar functionality:

- **kqueue (EVFILT_SIGNAL)**: We use the standard BSD `kqueue` mechanism with the `EVFILT_SIGNAL` filter. This allows us to receive signals through a file descriptor-like interface compatible with event loops.
- **ulock (Private API)**: For high-performance synchronization from signal handlers, we utilize the macOS private `ulock` system calls.
    - `SYS_ulock_wait` (515)
    - `SYS_ulock_wake` (516)

These provide a futex-like "wait-on-address" mechanism that is async-signal-safe, allowing a signal handler to wake worker threads with minimal overhead.

### 2. Synchronization Primitives

The implementation in `balls/darwin.nim` provides a shim for `signalfd_siginfo` and related functions, allowing the rest of the runner to remain platform-agnostic where possible while utilizing the most efficient kernel primitives available on Darwin.

## Rationale for Private APIs

While `kqueue` is the standard public API, `ulock` was chosen for the following reasons:

1. **Performance**: `ulock` provides the lowest possible latency for thread-to-thread signaling, equivalent to Linux futexes.
2. **Signal Safety**: Waking a thread via `ulock_wake` is safe to call from within a signal handler.
3. **Consistency**: This approach aligns with the strategy used in the working macOS port of `insideout`, allowing both projects to share a common architectural direction for Darwin support.

## Evolution of the Branch

The `macos-support` branch is intended to grow independently of the upstream Linux-centric implementation. By abstracting these low-level primitives into `balls/darwin.nim`, we ensure that `balls` can continue to leverage macOS-specific optimizations (such as `os_sync_wait_on_address` in newer macOS versions) without being constrained by Linux-specific API designs.

## Future Work

- Fully transition the concurrent runner to a `kqueue`-based event loop on macOS.
- Investigate the use of `qos` (Quality of Service) classes to better manage test execution priority on Apple Silicon.
