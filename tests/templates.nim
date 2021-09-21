import balls

suite "a test of setup/teardown":

  var n = 0

  block:
    check n == 0

  setup:
    checkpoint "running setup"
    inc n

  block:
    check n == 1

  teardown:
    checkpoint "running teardown"
    inc n

  block:
    check n == 2

  block:
    check n == 4

  teardown:
    checkpoint "le teardown has arrived"
    dec n

  block:
    check n == 6

  setup:
    checkpoint "new setup in da hizzous"
    n *= 2

  check n == 10
