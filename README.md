# testes [![Build Status](https://travis-ci.org/disruptek/testes.svg?branch=master)](https://travis-ci.org/disruptek/testes)

A small unittest macro with some features I needed.

## Why

Because `testament` and `testutils` are going nowhere fast.

## Goals

- better stack traces and test source output
- less magical syntax and less output omission
- aim to run many cheap tests in fewer files
- easily accumulate state between tests when desired
- when attached to a tty, errors are not fatal
- individual tests don't even have to compile
- replaces and enhances unittest at the same time
- easier test reordering, built-in timing, mem stats
- smaller, more self-evident testing macro

## Documentation
See [the documentation for the testes module](https://disruptek.github.io/testes/testes.html) as generated directly from the source.

## License
MIT
