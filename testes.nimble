version = "0.2.1"
author = "disruptek"
description = "a cure for salty testes"
license = "MIT"
requires "nim >= 1.0.4"
#requires "cligen >= 0.9.41 & <= 0.9.45"
requires "bump >= 1.8.18 & < 2.0.0"
#requires "https://github.com/disruptek/badresults < 2.0.0"
requires "https://github.com/disruptek/cutelog >= 1.1.0 & < 2.0.0"

proc execCmd(cmd: string) =
  echo "execCmd:" & cmd
  exec cmd

proc execTest(test: string) =
  execCmd "nim c -r " & test
  execCmd "nim c -d:danger -r " & test
  when (NimMajor, NimMinor) >= (1, 2):
    execCmd "nim c --gc:arc -r " & test
    execCmd "nim c --gc:arc -d:danger -r " & test

task test, "run tests for travis":
  execTest("tests/testicles.nim")

task docs, "generate some docs":
  exec "nim doc --project --outdir=docs testes.nim"
  exec "termtosvg docs/demo.svg --loop-delay=10000 --screen-geometry=80x60 --template=window_frame_powershell --command=\"nim c -r tests/testicles.nim\""
