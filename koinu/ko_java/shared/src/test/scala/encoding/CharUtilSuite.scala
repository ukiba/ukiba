package jp.ukiba.koinu.ko_java
package encoding

import munit.FunSuite

import java.nio.ByteBuffer
import java.nio.charset.{Charset, StandardCharsets}, StandardCharsets.{UTF_8, US_ASCII}

class CharUtilSuite extends FunSuite:
  private def byteBufferOf(bytes: Array[Byte]): ByteBuffer =
    ByteBuffer.wrap(bytes)

  test("decodePartial(ByteBuffer): empty input -> empty string, 0 remaining, position unchanged"):
    val in = byteBufferOf(Array.emptyByteArray)
    val pos0 = in.position
    val (s, rem) = CharUtil.decodePartial(in, UTF_8)
    assertEquals(s, "")
    assertEquals(rem, 0)
    assertEquals(in.position, pos0)

  test("decodePartial(ByteBuffer): ASCII / UTF-8 happy path consumes all bytes"):
    val bytes = "hello".getBytes(UTF_8)
    val in = byteBufferOf(bytes)
    val (s, rem) = CharUtil.decodePartial(in, UTF_8)
    assertEquals(s, "hello")
    assertEquals(rem, 0)
    assertEquals(in.position, bytes.length)

  test("decodePartial(ByteBuffer): trailing incomplete UTF-8 sequence leaves remaining bytes"):
    // '€' is E2 82 AC in UTF-8; provide only E2 82 (incomplete)
    val bytes = Array[Byte](0x41, 0xE2.toByte, 0x82.toByte) // "A" + partial '€'
    val in = byteBufferOf(bytes)

    val (s, rem) = CharUtil.decodePartial(in, UTF_8)

    assertEquals(s, "A")
    assertEquals(rem, 2)
    assertEquals(in.position, 1)

  test("decodePartial(ByteBuffer): malformed UTF-8 stops at first malformed sequence"):
    // 0xC3 expects a continuation byte in 0x80..0xBF; 0x28 is invalid continuation.
    val bytes = Array[Byte](0x41, 0xC3.toByte, 0x28.toByte) // "A" + malformed
    val in = byteBufferOf(bytes)

    val (s, rem) = CharUtil.decodePartial(in, UTF_8)

    assertEquals(s, "A")
    assertEquals(rem, 2)
    assertEquals(in.position, 1)

  test("decodePartial(ByteBuffer): unmappable (US-ASCII) stops at first unmappable byte"):
    val bytes = Array[Byte](0x41, 0x80.toByte) // "A" + non-ASCII byte
    val in = byteBufferOf(bytes)

    val (s, rem) = CharUtil.decodePartial(in, US_ASCII)

    assertEquals(s, "A")
    assertEquals(rem, 1)
    assertEquals(in.position, 1)

  test("decodePartial(ByteBuffer): maxBufSize smaller than output still decodes full content"):
    val s0 = "a" * 10_000
    val bytes = s0.getBytes(UTF_8)
    val in = byteBufferOf(bytes)

    val (s, rem) = CharUtil.decodePartial(in, UTF_8, maxBufSize = 1)

    assertEquals(s, s0)
    assertEquals(rem, 0)
    assertEquals(in.position, bytes.length)

  test("decodePartial(Array[Byte], offset, length): decodes only the requested slice"):
    val prefix = "xx".getBytes(UTF_8)
    val mid    = "hello".getBytes(UTF_8)
    val suffix = "yy".getBytes(UTF_8)

    val bytes = prefix ++ mid ++ suffix
    val offset = prefix.length
    val length = mid.length

    val (s, rem) = CharUtil.decodePartial(bytes, offset, length, UTF_8)
    assertEquals(s, "hello")
    assertEquals(rem, 0)

  test("decodePartial(Array[Byte], offset, length): invalid range throws"):
    val bytes = "hello".getBytes(UTF_8)

    intercept[IllegalArgumentException]:
      CharUtil.decodePartial(bytes, offset = -1, len = 1, UTF_8)

    intercept[IllegalArgumentException]:
      CharUtil.decodePartial(bytes, offset = 0, len = -1, UTF_8)

    intercept[IllegalArgumentException]:
      CharUtil.decodePartial(bytes, offset = bytes.length + 1, len = 0, UTF_8)

    intercept[IllegalArgumentException]:
      CharUtil.decodePartial(bytes, offset = 0, len = bytes.length + 1, UTF_8)

    intercept[IllegalArgumentException]:
      CharUtil.decodePartial(bytes, offset = bytes.length, len = 1, UTF_8)

  test("decodePartial(ByteBuffer): maxBufSize <= 0 throws"):
    val in = byteBufferOf("x".getBytes(UTF_8))

    intercept[IllegalArgumentException]:
      CharUtil.decodePartial(in, UTF_8, maxBufSize = 0)

    intercept[IllegalArgumentException]:
      CharUtil.decodePartial(in, UTF_8, maxBufSize = -1)

  test("decodePartial(ByteBuffer): non-zero position decodes only remaining bytes"):
    val bytes = "xxhello".getBytes(UTF_8)
    val in = byteBufferOf(bytes)
    in.position(2)
    val lim0 = in.limit

    val (s, rem) = CharUtil.decodePartial(in, UTF_8)

    assertEquals(s, "hello")
    assertEquals(rem, 0)
    assertEquals(in.position, bytes.length)
    assertEquals(in.limit, lim0)

  test("decodePartial(ByteBuffer): honors limit (slice-like behavior)"):
    val bytes = "hello!!".getBytes(UTF_8)
    val in = byteBufferOf(bytes)
    in.limit("hello".getBytes(UTF_8).length)

    val (s, rem) = CharUtil.decodePartial(in, UTF_8)

    assertEquals(s, "hello")
    assertEquals(rem, 0)
    assertEquals(in.position, in.limit)

  test("decodePartial(ByteBuffer): direct buffer happy path"):
    val bytes = "hello".getBytes(UTF_8)
    val in = ByteBuffer.allocateDirect(bytes.length)
    in.put(bytes)
    in.flip()

    val (s, rem) = CharUtil.decodePartial(in, UTF_8)

    assertEquals(s, "hello")
    assertEquals(rem, 0)
    assertEquals(in.position, bytes.length)

  test("decodePartial(ByteBuffer): malformed at first byte -> empty output, all bytes remain, position unchanged"):
    val bytes = Array[Byte](0xC3.toByte, 0x28.toByte) // malformed start
    val in = byteBufferOf(bytes)
    val pos0 = in.position

    val (s, rem) = CharUtil.decodePartial(in, UTF_8)

    assertEquals(s, "")
    assertEquals(rem, bytes.length)
    assertEquals(in.position, pos0)

  test("decodePartial(ByteBuffer): unmappable at first byte -> empty output, all bytes remain, position unchanged"):
    val bytes = Array[Byte](0x80.toByte)
    val in = byteBufferOf(bytes)
    val pos0 = in.position

    val (s, rem) = CharUtil.decodePartial(in, US_ASCII)

    assertEquals(s, "")
    assertEquals(rem, bytes.length)
    assertEquals(in.position, pos0)

  test("decodePartial(ByteBuffer): stops at first malformed and leaves subsequent valid bytes"):
    val prefix = "A€".getBytes(UTF_8)                       // valid (A + 3-byte char)
    val bad    = Array[Byte](0xC3.toByte, 0x28.toByte)      // malformed
    val tail   = "B".getBytes(UTF_8)                        // valid after error
    val bytes  = prefix ++ bad ++ tail
    val in = byteBufferOf(bytes)

    val (s, rem) = CharUtil.decodePartial(in, UTF_8)

    assertEquals(s, "A€")
    assertEquals(rem, bad.length + tail.length)
    assertEquals(in.position, prefix.length)

  test("decodePartial(ByteBuffer): trailing incomplete sequence via limit"):
    // "A" + start of '€' (E2 82 AC), but limit cuts after E2 82
    val bytes = Array[Byte](0x41, 0xE2.toByte, 0x82.toByte, 0xAC.toByte)
    val in = byteBufferOf(bytes)
    in.limit(3) // expose only "A" + E2 82

    val (s, rem) = CharUtil.decodePartial(in, UTF_8)

    assertEquals(s, "A")
    assertEquals(rem, 2)
    assertEquals(in.position, 1)
