import testes

expandMacros:
  testes:
    echo "welcome to testes"

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
      check r == 3
      dump r
      check r == 2

    block:
      ## unnamed block
      discard

    block:
      discard "unnamed test"

    block omission:
      skip()

    block broken:
      doesnt(compile)
