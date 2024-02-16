import std/strformat
import std/tables
import std/times

import std/logging
export logging

import ./time

type
  HiResLogger* = ref object of Logger
    origin*: Time
    origins*: Table[Level, Time]
    forward*: Logger

proc setOrigin*(logger: HiResLogger; level: Level; origin: Time) {.raises: [].} =
  logger.origins[level] = origin

proc setOrigins*(logger: HiResLogger; origin: Time) {.raises: [].} =
  for level in [lvlAll, lvlDebug, lvlInfo, lvlNotice, lvlWarn,
            lvlError, lvlFatal, lvlNone]:
    logger.setOrigin(level, origin)
