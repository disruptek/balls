import std/os
import std/exitprocs

import testes

expandMacros:
  testes:

    block goats:
      ## this is a test of goats
      discard

    block pigs:
      ## a test of pigs
      discard

    var r = 3

    block sometimes_the_wolf_is_nice:
      assert true
      inc r

    block sheepies:
      raise newException(ValueError, "you're terrible")

    block check_r:
      ## checking some things
      ## this block exists only to test inclusion of
      ## comments in the test code display...
      check r == 3
      dump r
      check r == 2

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
        testes:
          const compile = true
          proc doesnt(c: bool) =
            if not c:
              raise

          block:
            proc fixed() = doesnt(compile)
      else:
        testes:
          block:
            proc broken() = doesnt(compile)

    block assertions:
      assert 2 == 4 div 2
      assert 2 != 4 div 2

    assert "any statement is a test" != ""

if getEnv("TRAVIS_COMPILER", "unlikely") != "unlikely":
  setProgramResult QuitSuccess
