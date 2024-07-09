package jp.ukiba.koinu.ko_java.java8compat

import java.time.*

/** Provides Java 8 compatibilities for java.time classes */
package object time:
  // LocalTime
/*
  val HOURS_PER_DAY      = 24
  val MINUTES_PER_HOUR   = 60
  val SECONDS_PER_MINUTE = 60

  val MINUTES_PER_DAY = MINUTES_PER_HOUR * HOURS_PER_DAY
  val SECONDS_PER_HOUR = SECONDS_PER_MINUTE * MINUTES_PER_HOUR
  ...
*/

  // Duration
  opaque type DurationJava8Compat = Duration
  extension (duration: Duration)
    def java8compat: DurationJava8Compat = duration
  extension (duration: DurationJava8Compat)

    // since Java 9
    def toSeconds    : Long = duration.getSeconds

    def toDaysPart   : Long = duration.toSeconds / 24
    def toHoursPart  : Int  = (duration.toHours % 24).toInt
    def toMinutesPart: Int  = (duration.toMinutes % 60).toInt
    def toSecondsPart: Int  = (duration.toSeconds % 60).toInt
    def toMillisPart : Int  = duration.toNanosPart / 1_000_000
    def toNanosPart  : Int  = duration.getNano
