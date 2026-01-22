package jp.ukiba.koinu.ko_java
package encoding

import munit.FunSuite

import java.nio.ByteBuffer
import java.nio.charset.{Charset, StandardCharsets}, StandardCharsets.{UTF_8, US_ASCII}

class CharUtilJvmSuite extends FunSuite:
  private def byteBufferOf(bytes: Array[Byte]): ByteBuffer =
    ByteBuffer.wrap(bytes)

  // Scala.js 1.20.2: UnsupportedCharsetException: ISO-2022-JP
  private val `ISO-2022-JP` = Charset.forName("ISO-2022-JP")

  test("decodePartial(ByteBuffer): ISO-2022-JP roundtrip for complete input"):
    val s0 = "こんにちは世界"
    val bytes = s0.getBytes(`ISO-2022-JP`)
    val in = byteBufferOf(bytes)

    val (s, rem) = CharUtil.decodePartial(in, `ISO-2022-JP`)

    assertEquals(s, s0)
    assertEquals(rem, 0)
    assertEquals(in.position, bytes.length)

  test("decodePartial(ByteBuffer): ISO-2022-JP truncated input leaves remaining bytes"):
    val s0 = "こんにちは世界"
    val bytes = s0.getBytes(`ISO-2022-JP`)

    val in = byteBufferOf(bytes)
    in.limit(bytes.length - 1) // truncate by 1 byte

    val (s, rem) = CharUtil.decodePartial(in, `ISO-2022-JP`)

    assert(rem > 0)
    assertEquals(in.position + rem, in.limit)
    // optionally: assert(s.nonEmpty) but avoid if truncation may fall very early
