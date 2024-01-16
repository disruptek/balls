import std/algorithm

import balls
import balls/runner

suite "unit tests of the runner":
  test "shortPath":
    doAssert shortPath"tests/test.nim" == "test"
    doAssert shortPath"tests/taste.nim" == "aste"
    doAssert shortPath"tests/api/tread.nim" == "api/read"
    doAssert shortPath"examples/pipes.nim" == "examples/pipes"
    doAssert shortPath"more/api/tests.nim" == "more/api/tests"

  test "ordered":
    var pattern = makePattern testPattern
    var files = @["tests/trunner.nim",
                  "tests/test.nim",
                  "tests/ttemplates.nim"]
    sort files
    var found = ordered("tests", pattern)
    sort found
    doAssert found == files
