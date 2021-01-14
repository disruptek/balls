version = "1.1.11"
author = "disruptek"
description = "a cure for salty testes"
license = "MIT"

# requires kute & co.
requires "https://github.com/disruptek/grok >= 0.3.0 & < 1.0.0"
requires "https://github.com/disruptek/ups < 1.0.0"

bin = @["testes"]           # build the binary for basic test running
installExt = @["nim"]       # we need to install testes.nim also
skipDirs = @["tests"]       # so stupid...  who doesn't want tests?
#installFiles = @["testes.nim"] # https://github.com/nim-lang/Nim/issues/16661

task test, "run tests for ci":
  exec "nim c --run testes.nim"

task demo, "produce a demo":
  exec "nim c --define:release testes.nim"
  when (NimMajor, NimMinor) != (1, 0):
    echo "due to nim bug #16307, use nim-1.0"
    quit 1
  exec """demo docs/demo.svg "nim c --out=\$1 examples/balls.nim""""
  exec """demo docs/clean.svg "testes -f --out=\$1""""
