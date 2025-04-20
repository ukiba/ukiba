package jp.ukiba.koinu.ko_java
package time

class InstantSuite extends munit.FunSuite:
  test("epochMilli"):
    assertEquals                  (Instant.parse("+292278994-08-17T07:12:55.807Z").epochMilli, Long.MaxValue)
    intercept[ArithmeticException](Instant.parse("+292278994-08-17T07:12:55.808Z").epochMilli)

    assertEquals                  (Instant.parse("-292275055-05-16T16:47:04.192Z").epochMilli, Long.MinValue)
    intercept[ArithmeticException](Instant.parse("-292275055-05-16T16:47:04.191Z").epochMilli)

  test("epochMicro"):
    assertEquals                  (Instant.parse("+294247-01-10T04:00:54.775807Z").epochMicro, Long.MaxValue)
    intercept[ArithmeticException](Instant.parse("+294247-01-10T04:00:54.775808Z").epochMicro)

    assertEquals                  (Instant.parse("-290308-12-21T19:59:05.224192Z").epochMicro, Long.MinValue)
    intercept[ArithmeticException](Instant.parse("-290308-12-21T19:59:05.224191Z").epochMicro)

  test("epochNano"):
    assertEquals                  (Instant.parse("2262-04-11T23:47:16.854775807Z").epochNano, Long.MaxValue)
    intercept[ArithmeticException](Instant.parse("2262-04-11T23:47:16.854775808Z").epochNano)

    assertEquals                  (Instant.parse("1677-09-21T00:12:43.145224192Z").epochNano, Long.MinValue)
    intercept[ArithmeticException](Instant.parse("1677-09-21T00:12:43.145224191Z").epochNano)

  test("MaxEpochSecond"):
    assertEquals(Instant.MaxEpochSecond, Instant.MAX.epochSecond)

  test("MinEpochSecond"):
    assertEquals(Instant.MinEpochSecond, Instant.MIN.epochSecond)

  test("ofEpochMicro"):
    assertEquals(Instant.ofEpochMicro( 1_001_001), Instant.ofEpochMilli( 1_001).plusMicros(1))
    assertEquals(Instant.ofEpochMicro( 1_001_000), Instant.ofEpochMilli( 1_001))
    assertEquals(Instant.ofEpochMicro( 1_000_001), Instant.ofEpochMilli( 1_000).plusMicros(1))
    assertEquals(Instant.ofEpochMicro( 1_000_000), Instant.ofEpochMilli( 1_000))
    assertEquals(Instant.ofEpochMicro(     1_001), Instant.ofEpochMilli(     1).plusMicros(1))
    assertEquals(Instant.ofEpochMicro(     1_000), Instant.ofEpochMilli(     1))
    assertEquals(Instant.ofEpochMicro(         0), Instant.ofEpochMilli(     0))
    assertEquals(Instant.ofEpochMicro(    -1_000), Instant.ofEpochMilli(    -1))
    assertEquals(Instant.ofEpochMicro(    -1_001), Instant.ofEpochMilli(    -1).minusMicros(1))
    assertEquals(Instant.ofEpochMicro(-1_000_000), Instant.ofEpochMilli(-1_000))
    assertEquals(Instant.ofEpochMicro(-1_000_001), Instant.ofEpochMilli(-1_000).minusMicros(1))
    assertEquals(Instant.ofEpochMicro(-1_001_000), Instant.ofEpochMilli(-1_001))
    assertEquals(Instant.ofEpochMicro(-1_001_001), Instant.ofEpochMilli(-1_001).minusMicros(1))

  test("ofEpochNano"):
    assertEquals(Instant.ofEpochNano( 1_001_001_001), Instant.ofEpochMicro( 1_001_001).plusNanos(1))
    assertEquals(Instant.ofEpochNano( 1_001_001_000), Instant.ofEpochMicro( 1_001_001))
    assertEquals(Instant.ofEpochNano( 1_001_000_001), Instant.ofEpochMicro( 1_001_000).plusNanos(1))
    assertEquals(Instant.ofEpochNano( 1_001_000_000), Instant.ofEpochMicro( 1_001_000))
    assertEquals(Instant.ofEpochNano( 1_000_001_001), Instant.ofEpochMicro( 1_000_001).plusNanos(1))
    assertEquals(Instant.ofEpochNano( 1_000_001_000), Instant.ofEpochMicro( 1_000_001))
    assertEquals(Instant.ofEpochNano( 1_000_000_001), Instant.ofEpochMicro( 1_000_000).plusNanos(1))
    assertEquals(Instant.ofEpochNano( 1_000_000_000), Instant.ofEpochMicro( 1_000_000))
    assertEquals(Instant.ofEpochNano(     1_001_001), Instant.ofEpochMicro(     1_001).plusNanos(1))
    assertEquals(Instant.ofEpochNano(     1_001_000), Instant.ofEpochMicro(     1_001))
    assertEquals(Instant.ofEpochNano(     1_000_001), Instant.ofEpochMicro(     1_000).plusNanos(1))
    assertEquals(Instant.ofEpochNano(     1_000_000), Instant.ofEpochMicro(     1_000))
    assertEquals(Instant.ofEpochNano(         1_001), Instant.ofEpochMicro(         1).plusNanos(1))
    assertEquals(Instant.ofEpochNano(         1_000), Instant.ofEpochMicro(         1))
    assertEquals(Instant.ofEpochNano(             0), Instant.ofEpochMicro(         0))
    assertEquals(Instant.ofEpochNano(        -1_000), Instant.ofEpochMicro(        -1))
    assertEquals(Instant.ofEpochNano(        -1_001), Instant.ofEpochMicro(        -1).minusNanos(1))
    assertEquals(Instant.ofEpochNano(    -1_000_000), Instant.ofEpochMicro(    -1_000))
    assertEquals(Instant.ofEpochNano(    -1_000_001), Instant.ofEpochMicro(    -1_000).minusNanos(1))
    assertEquals(Instant.ofEpochNano(    -1_001_000), Instant.ofEpochMicro(    -1_001))
    assertEquals(Instant.ofEpochNano(    -1_001_001), Instant.ofEpochMicro(    -1_001).minusNanos(1))
    assertEquals(Instant.ofEpochNano(-1_000_000_000), Instant.ofEpochMicro(-1_000_000))
    assertEquals(Instant.ofEpochNano(-1_000_000_001), Instant.ofEpochMicro(-1_000_000).minusNanos(1))
    assertEquals(Instant.ofEpochNano(-1_000_001_000), Instant.ofEpochMicro(-1_000_001))
    assertEquals(Instant.ofEpochNano(-1_000_001_001), Instant.ofEpochMicro(-1_000_001).minusNanos(1))
    assertEquals(Instant.ofEpochNano(-1_001_000_000), Instant.ofEpochMicro(-1_001_000))
    assertEquals(Instant.ofEpochNano(-1_001_000_001), Instant.ofEpochMicro(-1_001_000).minusNanos(1))
    assertEquals(Instant.ofEpochNano(-1_001_001_000), Instant.ofEpochMicro(-1_001_001))
    assertEquals(Instant.ofEpochNano(-1_001_001_001), Instant.ofEpochMicro(-1_001_001).minusNanos(1))
