import balls

suite "suite balls":

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

  block sheepies:
    raise newException(ValueError, "you're terrible")

  block check_r:
    ## checking some things
    ## this block exists only to test inclusion of
    ## comments in the test code display...
    check r == 3
    echo r, " will not be output"

  block:
    ## check a list of statements in a block
    check "r should be 4":
      ## r less than
      r < 5
      ## r greater than
      r > 3

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
  const VariablesDefinedOutsideBlocksAreNotTests {.used.} = true

  test "a test: block is fine":
    discard

  block omission:
    skip()

  block:
    ## hide this gory when statement
    when defined(release):
      suite "fixed stuff":
        const compile = true
        proc doesnt(c: bool) =
          if not c:
            raise

        block:
          proc fixed() {.used.} = doesnt(compile)
    else:
      suite "broken stuff":
        block:
          proc broken() = doesnt(compile)

  block assertions:
    assert 2 == 4 div 2
    assert 2 != 4 div 2

  block omitted:
    skip("i just don't wanna")

  assert "any statement is a test" != ""
  check r > 0, $r & " is a good test of dynamic messages"

  report "report for expression expansion:", r != 5
  checkpoint "but checkpoint behaves as it does in unittest: ", r == 5

  block explicit_failure:
    fail("this looks bad")

  block check_with_message:
    let x = 0
    check "".len < x, "empty strings are STILL too long"

  block great_expectations:
    expect ValueError:
      checkpoint "you love to see it"
      raise newException(ValueError, "awful")

  block unmet_expectations:
    expect ValueError:
      checkpoint "here comes trouble"

  block dashed_expectations:
    expect ValueError:
      check false, "the truth hurts, but not as much as the false"
