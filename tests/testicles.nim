import testes

testes:
  echo "welcome to testes"

  block goats:
    ## this is a test of goats

  block pigs:
    ## a test of pigs

  block sometimes_the_wolf_is_nice:
    break

  block sheepies:
    raise newException(ValueError, "you're terrible")

  block:
    continue
