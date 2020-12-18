import testes

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
    check true, "that wolf is very nice"
    inc r

  block:
    ## unnamed block
    discard

  block:
    discard "unnamed test"

  block:
    ## check a list of statements in a block
    check:
      r < 5
      r > 3

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

  block omitted:
    skip("i just don't wanna")

  assert "any statement is a test" != ""
