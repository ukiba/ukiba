package jp.ukiba.koinu.ko_java
package encoding

import scala.annotation.tailrec
import java.nio.charset.{Charset, StandardCharsets}, StandardCharsets.{UTF_8, ISO_8859_1}

/**
  * Base 32 Encoding (RFC 4648)
  *
  * Base 32 encodes 5 bits to a character (5 binary bytes to 8 characters).

    input bits    01234567 89012345 67890123 45678901 23456789
    input bytes   ......b0 ......b1 ......b2 ......b3 ......b4
    encoded chars ...c0... c1...c2. ..c3...c 4...c5.. .c6...c7

    c0 bit mask   11111000                                     0xf8
    c1 bit mask   00000111 11000000                            0x07, 0xc0
    c2 bit mask            00111110                            0x3e
    c3 bit mask            00000001 11110000                   0x01, 0xf0
    c4 bit mask                     00001111 10000000          0x0f, 0x80
    c5 bit mask                              01111100          0x7c
    c6 bit mask                              00000011 11100000 0x03, 0xe0
    c7 bit mask                                       00011111 0x1f
  */
trait Base32 {
  /**
   * Encodes an array of bytes.
   * New line characters are not inserted.
   */
  def encode(bin: Seq[Byte]): String = {
    val numGroups = (bin.length + 4) / 5
    val encoded = new Array[Byte](numGroups * 8) // preallocate

    def encodeGroup(b0: Byte, b1: Byte = 0, b2: Byte = 0, b3: Byte = 0, b4: Byte = 0) = (
      encode((b0 & 0xf8) >> 3),
      encode((b0 & 0x07) << 2 | (b1 & 0xc0) >> 6),
      encode((b1 & 0x3e) >> 1),
      encode((b1 & 0x01) << 4 | (b2 & 0xf0) >> 4),
      encode((b2 & 0x0f) << 1 | (b3 & 0x80) >> 7),
      encode((b3 & 0x7c) >> 2),
      encode((b3 & 0x03) << 3 | (b4 & 0xe0) >> 5),
      encode( b4 & 0x1f),
    )

    @tailrec def loop(i: Int): Unit = { // `for (i <- 0 until numGroups)` allocates Range
      if (i < numGroups) {
        bin.length - 5 * i match {
          case j if (j >= 5) => // 5 bytes to 8 chars
            val (c0, c1, c2, c3, c4, c5, c6, c7) = encodeGroup(bin(5 * i), bin(5 * i + 1), bin(5 * i + 2), bin(5 * i + 3), bin(5 * i + 4))
            encoded(8 * i + 0) = c0
            encoded(8 * i + 1) = c1
            encoded(8 * i + 2) = c2
            encoded(8 * i + 3) = c3
            encoded(8 * i + 4) = c4
            encoded(8 * i + 5) = c5
            encoded(8 * i + 6) = c6
            encoded(8 * i + 7) = c7

          // TODO: maybe factoring out the below into a separate function results in higer instruction cache hit?
          case 4 => // 4 bytes to 7 chars
            val (c0, c1, c2, c3, c4, c5, c6,  _) = encodeGroup(bin(5 * i), bin(5 * i + 1), bin(5 * i + 2), bin(5 * i + 3))
            encoded(8 * i + 0) = c0
            encoded(8 * i + 1) = c1
            encoded(8 * i + 2) = c2
            encoded(8 * i + 3) = c3
            encoded(8 * i + 4) = c4
            encoded(8 * i + 5) = c5
            encoded(8 * i + 6) = c6
            encoded(8 * i + 7) = '='

          case 3 => // 3 bytes to 5 chars
            val (c0, c1, c2, c3, c4,  _,  _,  _) = encodeGroup(bin(5 * i), bin(5 * i + 1), bin(5 * i + 2))
            encoded(8 * i + 0) = c0
            encoded(8 * i + 1) = c1
            encoded(8 * i + 2) = c2
            encoded(8 * i + 3) = c3
            encoded(8 * i + 4) = c4
            encoded(8 * i + 5) = '='
            encoded(8 * i + 6) = '='
            encoded(8 * i + 7) = '='

          case 2 => // 2 bytes to 4 chars
            val (c0, c1, c2, c3,  _,  _,  _,  _) = encodeGroup(bin(5 * i), bin(5 * i + 1))
            encoded(8 * i + 0) = c0
            encoded(8 * i + 1) = c1
            encoded(8 * i + 2) = c2
            encoded(8 * i + 3) = c3
            encoded(8 * i + 4) = '='
            encoded(8 * i + 5) = '='
            encoded(8 * i + 6) = '='
            encoded(8 * i + 7) = '='

          case 1 => // 1 bytes to 2 chars
            val (c0, c1,  _,  _,  _,  _,  _,  _) = encodeGroup(bin(5 * i))
            encoded(8 * i + 0) = c0
            encoded(8 * i + 1) = c1
            encoded(8 * i + 2) = '='
            encoded(8 * i + 3) = '='
            encoded(8 * i + 4) = '='
            encoded(8 * i + 5) = '='
            encoded(8 * i + 6) = '='
            encoded(8 * i + 7) = '='
        }
        loop(i + 1)
      }
    }
    loop(0)

    new String(encoded, ISO_8859_1) // hopefully Compact Strings from Java 9
  }

  def encode(text: String, charset: Charset = UTF_8): String = encode(text.getBytes(charset))

  /** For Java */ def encode(bin: Array[Byte]): String = encode(bin.toSeq)
  /** For Java */ def encode(text: String): String = encode(text, UTF_8)

  /** Encodes a 5-bit value to a character */
  def encode(value: Int): Byte

  /**
   * Decodes an encoded text.
   * New line characters and padding are ignored.
   */
  def decode(text: String): Array[Byte] = {
    val encodedLen = { // `text.filter(ch => ch != '\n' && ch != '\r')` allocates another String
      val textLen = text.length
      @tailrec def loop(result: Int, i: Int): Int = {
        if (i < textLen) {
          val ch = text(i)
          if (ch != '=') {
            if (ch != '\n' && ch != '\r')
              loop(result + 1, i + 1)
            else
              loop(result, i + 1)
          } else
            result
        } else
          result
      }
      loop(0, 0)
    }

    val decodedLen = encodedLen % 8 match {
      case 0 => encodedLen / 8 * 5
      case 7 => encodedLen / 8 * 5 + 4
      case 5 => encodedLen / 8 * 5 + 3
      case 4 => encodedLen / 8 * 5 + 2
      case 2 => encodedLen / 8 * 5 + 1
      case _ => throw new IllegalArgumentException(s"illegal Base 32 encoded length: ${
          text.length} (${text.truncateWith(100, "...")})")
    }
    val decoded = new Array[Byte](decodedLen) // preallocate

    def decodeGroup(c0: Int, c1: Int = 0, c2: Int = 0, c3: Int = 0, c4: Int = 0, c5: Int = 0, c6: Int = 0, c7: Int = 0) = (
      (c0 << 3 | c1 >> 2).toByte,
      ((c1 & 0x03) << 6 | c2 << 1 | c3 >> 4).toByte,
      ((c3 & 0x0f) << 4 | c4 >> 1).toByte,
      ((c4 & 0x01) << 7 | c5 << 2 | c6 >> 3).toByte,
      ((c6 & 0x07) << 5 | c7).toByte,
    )

    @tailrec def loop(i: Int, j: Int): Unit = {
      if (i < encodedLen) {
        var i2 = i
        @tailrec def decodeNext: Int = {
          val ch = text(i2)
          i2 += 1
          if (ch != '\n' && ch != '\r')
            decode(ch)
          else
            decodeNext
        }

        encodedLen - i match {
          case k if (k >= 8) => // 8 chars to 5 bytes
            val (b0, b1, b2, b3, b4) = decodeGroup(decodeNext, decodeNext, decodeNext, decodeNext, decodeNext, decodeNext, decodeNext, decodeNext)
            decoded(j + 0) = b0
            decoded(j + 1) = b1
            decoded(j + 2) = b2
            decoded(j + 3) = b3
            decoded(j + 4) = b4

          case 7 => // 7 chars to 4 bytes
            val (b0, b1, b2, b3,  _) = decodeGroup(decodeNext, decodeNext, decodeNext, decodeNext, decodeNext, decodeNext, decodeNext)
            decoded(j + 0) = b0
            decoded(j + 1) = b1
            decoded(j + 2) = b2
            decoded(j + 3) = b3

          case 5 => // 5 chars to 3 bytes
            val (b0, b1, b2,  _,  _) = decodeGroup(decodeNext, decodeNext, decodeNext, decodeNext, decodeNext)
            decoded(j + 0) = b0
            decoded(j + 1) = b1
            decoded(j + 2) = b2

          case 4 => // 4 chars to 2 bytes
            val (b0, b1,  _,  _,  _) = decodeGroup(decodeNext, decodeNext, decodeNext, decodeNext)
            decoded(j + 0) = b0
            decoded(j + 1) = b1

          case 2 => // 2 chars to 1 bytes
            val (b0,  _,  _,  _,  _) = decodeGroup(decodeNext, decodeNext)
            decoded(j + 0) = b0
        }
        loop(i2, j + 5)
      }
    }
    loop(0, 0)

    decoded
  }

  /** Decodes a character to a 6-bit value */
  def decode(ch: Char): Int
}

/** Base 32 Encoding (RFC 4648) */
object Base32 extends Base32:
  /** RFC 4648 Table 3 */
  override def encode(value: Int) = value match
    case value if value < 0 => throw new MatchError(value)
    case value if value < 26 => ('A' + value).toByte
    case value if value < 32 => ('2' + (value - 26)).toByte

  /** Reverse table */
  override def decode(ch: Char) = ch match
    case ch if ch >= 'A' && ch <= 'Z' => ch - 'A'
    case ch if ch >= '2' && ch <= '7' => ch - '2' + 26

  object ExtendedHex extends Base32:
    /** RFC 4648 Table 4 */
    override def encode(value: Int) = value match
      case value if value < 0 => throw new MatchError(value)
      case value if value < 10 => ('0' + value).toByte
      case value if value < 32 => ('A' + (value - 10)).toByte

    /** Reverse table */
    override def decode(ch: Char) = ch match
      case ch if ch >= '0' && ch <= '9' => ch - '0'
      case ch if ch >= 'A' && ch <= 'V' => ch - 'A' + 10

  // TODO z-base-32

  // https://web.archive.org/web/20021223012947/http://www.crockford.com/wrmg/base32.html
  object Crockford extends Base32:
    val Alphabets = Array[Byte](
      '0', '1', '2', '3', '4', '5', '6', '7', '8', '9',
      'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H',
      'J', 'K',
      'M', 'N',
      'P', 'Q', 'R', 'S', 'T',
      'V', 'W', 'X', 'Y', 'Z',
    )

    override def encode(value: Int) = Alphabets(value)

    override def decode(ch: Char) = ch match
      case ch if ch >= '0' && ch <= '9' => ch - '0'
      case 'O' | 'o' => 0
      case 'I' | 'i' => 1
      case 'L' | 'l' => 1
      case ch if ch >= 'A' && ch <= 'H' => ch - 'A' + 10
      case ch if ch >= 'a' && ch <= 'h' => ch - 'a' + 10
      case 'J' | 'j' => 18
      case 'K' | 'k' => 19
      case 'M' | 'm' => 20
      case 'N' | 'n' => 21
      case ch if ch >= 'P' && ch <= 'T' => ch - 'P' + 22
      case ch if ch >= 'p' && ch <= 't' => ch - 'p' + 22
      case ch if ch >= 'V' && ch <= 'Z' => ch - 'V' + 27
      case ch if ch >= 'v' && ch <= 'z' => ch - 'v' + 27
