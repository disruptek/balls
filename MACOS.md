# macOS Native Support (Native & Private APIs)

This document outlines the architectural decisions and implementation details for supporting `balls` natively on macOS (Darwin).

## Context

The `balls` test runner originally depended on `insideout`, which relies heavily on Linux-specific features such as `sys/signalfd.h`, `epoll`, and `futex` syscalls. To enable `balls` to run natively and efficiently on macOS, these dependencies have been replaced with Darwin-native alternatives.

Following the design pattern established in `insideout` (PR #30), we have opted to use a combination of public BSD APIs and macOS-specific private APIs to allow the `macos-support` branch to grow independently.

## Implementation Details

### 1. Signal Handling (Replacement for `signalfd`)

MacOS does not have an equivalent to Linux's `signalfd`. To provide similar functionality:

- **kqueue (EVFILT_SIGNAL)**: We use the standard BSD `kqueue` mechanism with the `EVFILT_SIGNAL` filter. This allows us to receive signals through a file descriptor-like interface.
- **ulock (Private API)**: For high-performance synchronization from signal handlers, we utilize the macOS private `ulock` system calls.
    - `SYS_ulock_wait` (515)
    - `SYS_ulock_wake` (516)

These provide a futex-like "wait-on-address" mechanism that is async-signal-safe, allowing a signal handler to wake worker threads with minimal overhead.

### 2. Parallel Execution Engine

Instead of relying on the Linux-centric `insideout` scheduler, the macOS runner uses a native parallel execution engine:

- **Threadpool**: Utilizes Nim's `std/threadpool` to manage a pool of worker threads for concurrent test compilation and execution.
- **Shared Matrix**: A thread-safe results matrix protected by `std/locks` ensuring atomic updates from multiple workers.

### 3. Apple Silicon Optimization (QoS)

To maximize performance on modern Mac hardware (M1/M2/M3 chips):

- **Quality of Service (QoS)**: We utilize `pthread_set_qos_class_self_np` to assign the `QOS_CLASS_USER_INITIATED` class to worker threads.
- **P-core Preference**: By setting this QoS class, the macOS scheduler prioritizes test tasks on **Performance cores (P-cores)** rather than Efficiency cores (E-cores), significantly reducing total test execution time.

## Rationale for Private APIs

While `kqueue` is the standard public API, `ulock` was chosen for the following reasons:

1. **Performance**: `ulock` provides the lowest possible latency for thread-to-thread signaling, equivalent to Linux futexes.
2. **Signal Safety**: Waking a thread via `ulock_wake` is safe to call from within a signal handler.
3. **Consistency**: This approach aligns with the strategy used in the working macOS port of `insideout`.

## Future Work

- Fully transition the event monitoring to a `kqueue` `EVFILT_PROC` loop for even more granular control over process termination.
- Implement memory pressure handling using `DISPATCH_SOURCE_TYPE_MEMORYPRESSURE`.
