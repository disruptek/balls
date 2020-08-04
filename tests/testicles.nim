import testes

expandMacros:
  testes:

    block goats:
      ## this is a test of goats

    block pigs:
      ## a test of pigs

    var r = 3

    block sometimes_the_wolf_is_nice:
      break
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

    test "a test: block":
      discard

    block omission:
      skip()

    block:
      proc broken() =
        ## this test should be named `broken`
        doesnt(compile)
