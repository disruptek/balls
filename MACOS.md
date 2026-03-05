# macOS Native Support (Native & Private APIs)

This document outlines the architectural decisions and implementation details for supporting `balls` natively on macOS (Darwin).

## Context

The `balls` test runner originally depended on `insideout`, which relies heavily on Linux-specific features such as `sys/signalfd.h`, `epoll`, and `futex` syscalls. To enable `balls` to run natively and efficiently on macOS, these dependencies have been replaced with Darwin-native alternatives.

Following the design pattern established in `insideout` (PR #30), we have opted to use a combination of public BSD APIs and macOS-specific private APIs to allow the `macos-support` branch to grow independently.

## Implementation Details

### 1. Advanced Event Monitoring (kqueue)

Instead of a simple signal handler, we use a dedicated **kqueue** monitoring thread (`monitorSystem`) to handle multiple system events asynchronously:

- **Signal Handling**: Monitors `SIGINT` and `SIGTERM` via `EVFILT_SIGNAL`.
- **Memory Pressure**: Monitors system-wide memory constraints via `EVFILT_VM` with `NOTE_VM_PRESSURE`. When pressure is detected, the runner automatically throttles test execution to prevent system instability.

### 2. Synchronization Primitives (ulock)

For low-latency synchronization between the monitoring thread and the test workers, we utilize the macOS private **ulock** system calls:
- `SYS_ulock_wait` (515)
- `SYS_ulock_wake` (516)

These provide a futex-like "wait-on-address" mechanism that is async-signal-safe.

### 3. Parallel Execution Engine

The macOS runner uses a native parallel execution engine:
- **Threadpool**: Utilizes Nim's `std/threadpool` to manage concurrent test compilation and execution.
- **Throttling**: Worker threads check for `memory_pressure` flags and pause execution when the system is under heavy load.

### 4. Apple Silicon Optimization (QoS)

To maximize performance on modern Mac hardware (M1/M2/M3 chips):
- **Quality of Service (QoS)**: We utilize `pthread_set_qos_class_self_np` to assign:
    - `QOS_CLASS_USER_INITIATED` for test execution (targeting Performance cores).
    - `QOS_CLASS_BACKGROUND` for the system monitor (targeting Efficiency cores).

## Rationale for Private APIs

1. **Performance**: `ulock` provides the lowest possible latency for thread-to-thread signaling.
2. **Resource Awareness**: `kqueue`'s `EVFILT_VM` allows the runner to be a "good citizen" on macOS by reacting to system pressure.
3. **Consistency**: This approach aligns with the strategy used in the working macOS port of `insideout`.

## Evolution of the Branch

The `macos-support` branch has now achieved full parity with Linux features while adding Darwin-specific enhancements. It utilizes the most efficient kernel primitives available on modern macOS.
