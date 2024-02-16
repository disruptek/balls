import std/json
import std/times

import ./time

proc unixTimeToZone*(js: JsonNode; tz: Timezone; name = "time"): DateTime =
  result = js{name}.getInt(0).fromUnix().inZone(tz)

converter `%`*(input: DateTime): JsonNode =
  result = % input.iso8601()
