package jp.ukiba.koinu.ko_java

import munit.FunSuite

import java.io.ByteArrayInputStream
import java.nio.charset.StandardCharsets

class HexSuite extends FunSuite:

  test("dump(Array[Byte]) renders 0x00..0x1f like hexdump -C"):

    val bytes = Array.tabulate(32)(i => i.toByte)

    val expected =
      List(
        "00000000  00 01 02 03 04 05 06 07  08 09 0a 0b 0c 0d 0e 0f  |................|",
        "00000010  10 11 12 13 14 15 16 17  18 19 1a 1b 1c 1d 1e 1f  |................|"
      )

    assertEquals(Hex.dump(bytes).toList, expected)

  test("dump(Array[Byte], offset, length) uses logical offset starting from 0"):

    val bytes = Array.tabulate(32)(i => i.toByte)

    val expected =
      List(
        "00000000  10 11 12 13 14 15 16 17  18 19 1a 1b 1c 1d 1e 1f  |................|"
      )

    assertEquals(Hex.dump(bytes, offset = 16, length = 16).toList, expected)

  test("dump(InputStream) renders a partial final line with correct padding"):

    val bytes = "Hello, world!\n".getBytes(StandardCharsets.US_ASCII)
    val in = ByteArrayInputStream(bytes)

    val expected =
      List(
        "00000000  48 65 6c 6c 6f 2c 20 77  6f 72 6c 64 21 0a        |Hello, world!.  |"
      )

    assertEquals(Hex.dump(in).toList, expected)

  test("dump(Iterator[Byte]) matches dump(Array[Byte]) for same content"):

    val bytes = Array.tabulate(32)(i => i.toByte)

    val expected =
      List(
        "00000000  00 01 02 03 04 05 06 07  08 09 0a 0b 0c 0d 0e 0f  |................|",
        "00000010  10 11 12 13 14 15 16 17  18 19 1a 1b 1c 1d 1e 1f  |................|"
      )

    assertEquals(Hex.dump(bytes.iterator).toList, expected)

  test("dump(IterableOnce[Byte]) matches dump(Array[Byte]) for same content"):

    val bytes = Array.tabulate(32)(i => i.toByte)
    val iterableOnce: IterableOnce[Byte] = bytes.toIndexedSeq

    val expected =
      List(
        "00000000  00 01 02 03 04 05 06 07  08 09 0a 0b 0c 0d 0e 0f  |................|",
        "00000010  10 11 12 13 14 15 16 17  18 19 1a 1b 1c 1d 1e 1f  |................|"
      )

    assertEquals(Hex.dump(iterableOnce).toList, expected)

  test("repeated full lines collapse into a single '*' line"):

    val bytes = Array.fill(48)(0.toByte) // 3 identical 16-byte blocks

    val expected =
      List(
        "00000000  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|",
        "*"
      )

    assertEquals(Hex.dump(bytes).toList, expected)

  test("empty input yields an empty iterator"):

    assertEquals(Hex.dump(Array.emptyByteArray).toList, List.empty[String])

  test("next() on an empty dump iterator throws NoSuchElementException"):

    val it = Hex.dump(Array.emptyByteArray)
    intercept[NoSuchElementException]:
      it.next()
