import std/algorithm

import balls
import balls/runner

suite "unit tests of the runner":
  test "ordered":
    var pattern = makePattern testPattern
    var files = @["tests/trunner.nim",
                  "tests/test.nim",
                  "tests/ttemplates.nim"]
    sort files
    var found = ordered("tests", pattern)
    sort found
    doAssert found == files
