import balls

suite "a pretty suite shim":

  block goats:
    ## this is a test of goats
    discard

  block pigs:
    ## a test of pigs
    discard

  var r = 3

  block sometimes_the_wolf_is_nice:
    assert true
    check true, "that wolf is very nice"
    inc r

  block:
    ## unnamed block
    discard

  block:
    discard "unnamed test"

  block:
    ## check a list of statements in a block
    check "something failed":
      ## r less than
      r < 5
      ## r greater than
      r > 3

  inc r
  check r > 0
  ## this is a nice comment
  type TypesAreNotTests {.used.} = bool
  ## there's really nothing else to say
  const VariablesDefinedOutsideBlocksAreNotTests {.used.} = true

  test "a test: block is fine":
    discard

  block omission:
    skip()

  block omitted:
    skip("i just don't wanna")

  block mostOmitted:
    skip "i really just don't wanna":
      {.error: "never got here".}

  assert "any statement is a test" != ""
  check r > 0, $r & " is a good test of dynamic messages"

  report "report for expression expansion:", r != 5
  checkpoint "but checkpoint behaves as it does in unittest: ", r == 5

  block great_expectations:
    expect ValueError:
      checkpoint "you love to see it"
      raise newException(ValueError, "awful")
