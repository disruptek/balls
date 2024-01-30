# balls

[![Test Matrix](https://github.com/disruptek/balls/workflows/CI/badge.svg)](https://github.com/disruptek/balls/actions?query=workflow%3ACI)
[![GitHub release (latest by date)](https://img.shields.io/github/v/release/disruptek/balls?style=flat)](https://github.com/disruptek/balls/releases/latest)
![Minimum supported Nim version](https://img.shields.io/badge/nim-1.7.3%2B-informational?style=flat&logo=nim)
[![License](https://img.shields.io/github/license/disruptek/balls?style=flat)](#license)

This project contains two things:
- a unit test dsl that helps debug tests that do not compile
- a test runner that smartly expands the test matrix

## Why?

There's a want to centralize and reuse the logic that determines what code is
deemed successful and how we go about measuring this.

## Goals

- expect everything to work; any statement is a test
- show the test code as we run it, unless not :wink:
- better stack traces and test source output
- less magical syntax and less output omission
- aim to run many cheap tests in fewer files
- easily accumulate state between tests when desired
- when attached to a tty, errors are not fatal
- individual tests don't even have to compile
- easier test reordering, built-in timing, mem stats
- smaller, more self-evident testing macro
- no options means, "what would disruptek do?"

## FAQ

If you're wondering why you cannot see compiler error messages for a particular
test that fails to compile, note that we use `compiles()` to swallow errors and
allow subsequent tests to get executed, but only outside of `--define:release`.

- With `--define:release`, compiler errors are fatal and output to console.
- With `--define:ballsDry`, no color or emojis are output.
- With `--define:ballsNoDump`, no symbol dumps will occur on test failure

## Test Runner Usage

You can now run `balls` to run a limited local test matrix against the current
compiler -- the matrix is expanded automatically on GitHub Actions CI. This
runtime will try to guess how to test your project regardless of its structure,
but you can help narrow what it chooses to compile by...

- placing files matching `t*.nim` under a `tests` subdirectory, or
- specifying the test directory or filename, or
- passing arguments to balls which are expanded via globbing:

```
$ balls 'examples/***'  # test all files anywhere beneath examples sub-directory
$ balls experiments     # test all files below the experiments sub-directory
$ balls tests/tfoo      # test the tests/tfoo.nim
$ balls '**/trunner'    # test trunner.nim wherever it may live
```

You can `--define:ballsPatterns=regex` to use regular expressions in these
patterns instead of the simpler glob syntax.

The test runner is threaded and runs a comprehensive matrix which tries to
safely reuse compilation assets.

![runner](docs/runner.svg "test runner")

You can add arguments which will be passed directly to the compiler:

```
$ balls --styleCheck:error
# ... all tests are compiled with --styleCheck:error ...
```

You can specify memory models with which to restrict the test matrix:
```
$ balls --mm:arc --mm:orc
# ... tests are run only with --mm:arc and --mm:orc ...
```

You can similarly constrain backends and optimizations:
```
$ balls --backend:c --define:danger
# ... tests are run only with the c backend, and danger optimizations ...
```

By default, failing tests that are expected to pass will cause
early termination of the test runner, skipping any remaining test
invocations. You can disable this behavior by building `balls` with
`--define:ballsFailFast=off`.

Set `BALLS_CORES` in your process environment to a positive integer to
constrain compilation and test execution to a certain amount of concurrency.

## Valgrind and Sanitizers

When `--define:danger` test builds are part of the matrix, we will also
attempt runtime analysis on the tests to check for memory errors and races. If
`valgrind` is found in the environment, it can be used as well.

### Compile-time Toggles:
  - `--define:ballsUseValgrind=off` to never use valgrind
  - `--define:ballsUseSanitizers=off` to never use sanitizers

### Environmental Variables:
  - `BALLS_VALGRIND_FLAGS` a string of arguments to add to valgrind
  - `BALLS_VALGRIND` a boolean to enable or disable valgrind use
  - `BALLS_SANITIZERS` a boolean to enable or disable sanitizer use

Simi

## Test Library Usage

Here's a set of example tests which will pass (and fail) in interesting ways.

```nim
import balls

suite "suite balls":

  setup:
    discard "setup blocks work like you expect"

  block goats:
    ## this is a test of goats
    discard

  setup:
    discard "also, you can redefine them whenever"

  block pigs:
    ## a test of pigs
    discard

  teardown:
    discard "teardown works the same way, of course"

  var r = 3

  block sometimes_the_wolf_is_nice:
    assert true
    check true, "that wolf is very nice"
    inc r

  block sheepies:
    raise newException(ValueError, "you're terrible")

  block check_r:
    ## checking some things
    ## this block exists only to test inclusion of
    ## comments in the test code display...
    check r == 3
    echo r, " will not be output"

  block:
    ## check a list of statements in a block
    check "r should be 4":
      r < 5
      r > 3

  block:
    ## unnamed block
    discard

  block:
    discard "unnamed test"

  inc r
  assert r > 0
  ## this is a nice comment
  type TypesAreNotTests = bool
  ## there's really nothing else to say
  const VariablesDefinedOutsideBlocksAreNotTests = true

  test "a test: block is fine":
    discard

  block omission:
    skip()

  block:
    ## hide this gory when statement
    when defined(release):
      suite "fixed stuff":
        const compile = true
        proc doesnt(c: bool) =
          if not c:
            raise

        block:
          proc fixed() = doesnt(compile)
    else:
      suite "broken stuff":
        block:
          proc broken() = doesnt(compile)

  block assertions:
    assert 2 == 4 div 2
    assert 2 != 4 div 2

  block omitted:
    skip("i just don't wanna")

  assert "any statement is a test" != ""
  check r > 0, $r & " is a good test of dynamic messages"

  report "report for expression expansion:", r != 5
  checkpoint "but checkpoint behaves as it does in unittest: ", r == 5

  block explicit_failure:
    fail("this looks bad")

  block check_with_message:
    let x = 0
    check "".len < x, "empty strings are STILL too long"

  block great_expectations:
    expect ValueError:
      checkpoint "you love to see it"
      raise newException(ValueError, "awful")

  block unmet_expectations:
    expect ValueError:
      checkpoint "here comes trouble"

  block dashed_expectations:
    expect ValueError:
      check false, "the truth hurts, but not as much as the false"
```

Relax; your tests won't usually be this chaotic...  Right?  😉

![demonstration](docs/demo.svg "demonstration")

Here's a similar demo with `--define:danger` enabled to show the
performance metrics; no failing tests are permitted in such a build.

![performance](docs/clean.svg "performance")

## Documentation
See [the documentation for the balls module](https://disruptek.github.io/balls/balls.html) as generated directly from the source.

## License
MIT
