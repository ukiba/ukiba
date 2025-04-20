package jp.ukiba.koinu.ko_java
package java8compat

import java8compat.time.java8compat

import java.time.Duration

class DurationJava8CompatSuite extends munit.FunSuite:
  // remove .java8compat to tests the JDK classes
  val Zero = Duration.ZERO.java8compat
  val AllPart1 = Duration.ZERO.plusDays(1).plusHours(1).plusMinutes(1).plusSeconds(1)
      .plusMillis(1).plusNanos(1).java8compat
  val AllPartMax = Duration.ZERO.plusDays(999).plusHours(23).plusMinutes(59).plusSeconds(59)
      .plusMillis(999).plusNanos(999_999).java8compat

  test("Zero"):
    assertEquals(Zero.toSeconds    , 0L)
    assertEquals(Zero.toNanosPart  , 0)

    assertEquals(Zero.toDaysPart   , 0L)
    assertEquals(Zero.toHoursPart  , 0)
    assertEquals(Zero.toMinutesPart, 0)
    assertEquals(Zero.toSecondsPart, 0)
    assertEquals(Zero.toMillisPart , 0)

    assertEquals(Zero.isPositive   , false)

  test("AllPart1"):
    assertEquals(AllPart1.toSeconds    , ((1L * 24 + 1) * 60 + 1) * 60 + 1)
    assertEquals(AllPart1.toNanosPart  , 1_000_001)

    assertEquals(AllPart1.toDaysPart   , 1L)
    assertEquals(AllPart1.toHoursPart  , 1)
    assertEquals(AllPart1.toMinutesPart, 1)
    assertEquals(AllPart1.toSecondsPart, 1)
    assertEquals(AllPart1.toMillisPart , 1)

    assertEquals(AllPart1.isPositive   , true)

  test("AllPartMax"):
    assertEquals(AllPartMax.toSeconds    , ((999L * 24 + 23) * 60 + 59) * 60 + 59)
    assertEquals(AllPartMax.toNanosPart  , 999_999_999)

    assertEquals(AllPartMax.toDaysPart   , 999L)
    assertEquals(AllPartMax.toHoursPart  , 23)
    assertEquals(AllPartMax.toMinutesPart, 59)
    assertEquals(AllPartMax.toSecondsPart, 59)
    assertEquals(AllPartMax.toMillisPart , 999)

    assertEquals(AllPartMax.isPositive   , true)
