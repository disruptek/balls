version = "0.0.1"
author = "disruptek"
description = "a cure for salty testes"
license = "MIT"
requires "nim >= 1.0.4"
#requires "cligen >= 0.9.41 & <= 0.9.45"
#requires "bump >= 1.8.18 & < 2.0.0"
#requires "https://github.com/disruptek/badresults < 2.0.0"
#requires "https://github.com/disruptek/cutelog >= 1.1.0 & < 2.0.0"

backend = "c"

proc execCmd(cmd: string) =
  echo "execCmd:" & cmd
  exec cmd

proc execTest(test: string) =
  execCmd "nim c --gc:arc -r " & test
  execCmd "nim c --gc:arc -d:danger -r " & test

task test, "run tests for travis":
  execTest("tests/testicles.nim")
