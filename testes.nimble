version = "0.3.2"
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
  when getEnv("GITHUB_ACTIONS", "false") != "true":
    execCmd "nim c -r " & test
    execCmd "nim c --gc:arc -d:danger -r " & test
  else:
    execCmd "nim c              -r " & test
    execCmd "nim cpp            -r " & test
    execCmd "nim c   -d:danger  -r " & test
    execCmd "nim cpp -d:danger  -r " & test
    when (NimMajor, NimMinor) >= (1, 2):
      execCmd "nim c --useVersion:1.0 -d:danger -r " & test
      execCmd "nim c   --gc:arc -d:danger -r " & test
      execCmd "nim cpp --gc:arc -d:danger -r " & test

task test, "run tests for ci":
  execTest("tests/testicles.nim")

task docs, "generate some docs":
  exec "termtosvg docs/demo.svg --loop-delay=10000 --screen-geometry=80x60 --template=window_frame_powershell --command=\"nim c -r tests/balls.nim\""
