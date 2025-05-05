package jp.ukiba.koinu.ko_java

import scala.concurrent.duration.{FiniteDuration, NANOSECONDS, DurationLong}
import java.time.{Instant, ZonedDateTime, ZoneOffset, OffsetDateTime, Duration => JDuration}, ZoneOffset.UTC

/** Extensions to java.time package */
package object time:
  extension (instant: Instant)
    inline def atUtcZone  : ZonedDateTime  = instant.atZone  (UTC)
    inline def atUtcOffset: OffsetDateTime = instant.atOffset(UTC)

    /**
     * throws No exception in [Instant.MIN, Instant.MAX]
     */
    inline def epochSecond: Long = instant.getEpochSecond

    /**
     * @throws java.lang.ArithmeticException outside [
     *   `-292275055-05-16T16:47:04.192Z`,
     *   `+292278994-08-17T07:12:55.807Z`
     * ]
     */
    def epochMilli: Long = instant.toEpochMilli

    /**
     * @throws java.lang.ArithmeticException outside [
     *   `-290308-12-21T19:59:05.224192Z`,
     *   `+294247-01-10T04:00:54.775807Z`
     * ]
     */
    def epochMicro: Long =
      val epochSecond = instant.getEpochSecond
      if epochSecond >= 0 then
        Math.addExact(
          Math.multiplyExact(epochSecond, 1_000_000),
          Math.floorDiv(instant.getNano, 1_000)
        )
      else
        Math.addExact(
          Math.multiplyExact(epochSecond + 1, 1_000_000), // `+ 1` to avoid overflow
          Math.floorDiv(instant.getNano, 1_000) - 1_000_000
        )

    /**
     * @throws java.lang.ArithmeticException outside [
     *   `1677-09-21T00:12:43.145224192Z`,
     *   `2262-04-11T23:47:16.854775807Z`
     * ]
     */
    def epochNano: Long =
      val epochSecond = instant.getEpochSecond
      if epochSecond >= 0 then
        Math.addExact(
          Math.multiplyExact(epochSecond, 1_000_000_000),
          instant.getNano
        )
      else
        Math.addExact(
          Math.multiplyExact(epochSecond + 1, 1_000_000_000), // `+ 1` to avoid overflow
          instant.getNano - 1_000_000_000
        )

    def plusMicros(microsToAdd: Long): Instant =
      instant
          .plusMillis(Math.floorDiv(microsToAdd, 1_000))
          .plusNanos (Math.multiplyExact(Math.floorMod(microsToAdd, 1_000), 1_000))

    def minusMicros(microsToAdd: Long): Instant =
      if microsToAdd == Long.MinValue then
        plusMicros(Long.MaxValue).plusMicros(1) // cannot negate when MinValue
      else
        plusMicros(-microsToAdd)

  // TODO use `extension (erased $: Instant.type)` on Scala 3.6.5+
  // https://github.com/lampepfl/dotty-feature-requests/issues/168#issuecomment-1486536624
  type Instant = java.time.Instant
  object Instant { export java.time.Instant.* }
  extension ($: Instant.type)
    // MAX is +1000000000-12-31T23:59:59.999999999Z
    // MIN is -1000000000-01-01T00:00Z

    /**
     * `31_556_889_864_403_199`, which can be calculated from `MAX` as
     *
     *      val years = 1_000_000_000L - 1970
     *      val leapYears = years / 4 - years / 100 + years / 400 + 1
     *      val days = years * 365 + leapYears + 364
     *      val seconds = ((days * 24 + 23) * 60 + 59) * 60 + 59
     */
    def MaxEpochSecond: Long = 31_556_889_864_403_199L

    /**
     * `-31_557_014_167_219_200`, which can be calculated from `MIN` as
     *
     *      val years = -1_000_000_000L - 1970
     *      val leapYears = years / 4 - years / 100 + years / 400 - 1
     *      val days = years * 365 + leapYears
     *      val seconds = days * 24 * 60 * 60
     */
    def MinEpochSecond: Long = -31_557_014_167_219_200L

    /**
     * Obtains an `Instant` given microseconds from `EPOCH`.
     * The microseconds can represents `-290308-12-21T19:59:05.224192Z` to `294247-01-10T04:00:54.775807Z`.
     *
     * Like `ofEpochMilli`, this method
     * 1. throws `ArithmeticException` when overflow
     * 2. rounds toward negative infinity rather than 0 when negative
     * 3. cannot cover the entire range from `Instant.MIN` to `Instant.MAX` (1_000_000_000 years)
     */
    def ofEpochMicro(epochMicro: Long): Instant =
      val epochSecond = Math.floorDiv(epochMicro, 1000_000)
      val micro       = Math.floorMod(epochMicro, 1000_000)
      Instant.ofEpochSecond(epochSecond, micro * 1000)

    /**
     * Obtains an `Instant` given nanoseconds from `EPOCH`.
     * The nanoseconds can represents `1677-09-21T00:12:43.145224192Z` to `2262-04-11T23:47:16.854775807Z`.
     *
     * Like `ofEpochMilli`, this method
     * 1. throws `ArithmeticException` when overflow
     * 2. rounds toward negative infinity rather than 0 when negative
     * 3. cannot cover the entire range from `Instant.MIN` to `Instant.MAX` (1_000_000_000 years)
     */
    def ofEpochNano(epochNano: Long): Instant = Instant.ofEpochSecond(0, epochNano)

    // fs2.io.file.Files.getLastModifiedTime returns FiniteDuration
    def ofEpochDuration(duration: FiniteDuration): Instant =
      val wholeSecs = duration.toSeconds
      Instant.ofEpochSecond(wholeSecs, (duration - wholeSecs.seconds).toNanos)

  extension (duration: JDuration)
    def toScala: FiniteDuration = FiniteDuration(duration.toNanos, NANOSECONDS)
