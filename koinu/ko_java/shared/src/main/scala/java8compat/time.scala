package jp.ukiba.koinu.ko_java
package java8compat

import java.time.*

/** Provides Java 8 compatibilities for java.time classes */
object time:
  // Duration
  opaque type DurationJava8Compat = Duration
  extension (duration: Duration)
    def java8compat: DurationJava8Compat = duration
  extension (duration: DurationJava8Compat)
    // since Java 9
    inline def toSeconds  : Long = duration.getSeconds
    inline def toNanosPart: Int  = duration.getNano

    def toDaysPart   : Long =   toSeconds / 86400
    def toHoursPart  : Int  = ((toSeconds /  3600) % 24).toInt
    def toMinutesPart: Int  = ((toSeconds /    60) % 60).toInt
    def toSecondsPart: Int  =  (toSeconds          % 60).toInt
    def toMillisPart : Int  = toNanosPart / 1_000_000

    // TODO dividedBy(Duration)
    // TODO truncatedTo(TemporalUnit)

    // since Java 18
    def isPositive: Boolean = toSeconds > 0 || toNanosPart > 0
