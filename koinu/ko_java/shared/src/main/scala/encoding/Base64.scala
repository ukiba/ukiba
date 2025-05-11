package jp.ukiba.koinu.ko_java
package encoding

import scala.annotation.tailrec
import java.nio.charset.{Charset, StandardCharsets}, StandardCharsets.{UTF_8, ISO_8859_1}

/**
  * Base 64 Encoding (RFC 4648)
  *
  * Base 64 encodes 6 bits to a character (3 binary bytes to 4 characters).

    input bits    01234567 89012345 67890123
    input bytes   ......b0 ......b1 ......b2
    encoded chars ....c0.. ..c1.... c2....c3
    c0 bit mask   11111100                   0xfc
    c1 bit mask   00000011 11110000          0x03, 0xf0
    c2 bit mask            00001111 11000000 0x0f, 0xc0
    c3 bit mask                     00111111 0x3f
  */
trait Base64 {
  /**
   * Encodes an array of bytes.
   * New line characters are not inserted.
   */
  def encode(bin: Seq[Byte]): String = {
    val numGroups = (bin.length + 2) / 3
    val encoded = new Array[Byte](numGroups * 4) // preallocate

    def encodeGroup(b0: Byte, b1: Byte = 0, b2: Byte = 0) = (
      encode((b0 & 0xfc) >> 2),
      encode((b0 & 0x03) << 4 | (b1 & 0xf0) >> 4),
      encode((b1 & 0x0f) << 2 | (b2 & 0xc0) >> 6),
      encode( b2 & 0x3f),
    )

    @tailrec def loop(i: Int): Unit = { // `for (i <- 0 until numGroups)` allocates Range
      if (i < numGroups) {
        bin.length - 3 * i match {
          case j if (j >= 3) => // 3 bytes to 4 chars
            val (c0, c1, c2, c3) = encodeGroup(bin(3 * i), bin(3 * i + 1), bin(3 * i + 2))
            encoded(4 * i + 0) = c0
            encoded(4 * i + 1) = c1
            encoded(4 * i + 2) = c2
            encoded(4 * i + 3) = c3

          // TODO: maybe factoring out the below into a separate function results in higer instruction cache hit?
          case 2 => // 2 bytes to 3 chars
            val (c0, c1, c2,  _) = encodeGroup(bin(3 * i), bin(3 * i + 1))
            encoded(4 * i + 0) = c0
            encoded(4 * i + 1) = c1
            encoded(4 * i + 2) = c2
            encoded(4 * i + 3) = '='

          case 1 => // 1 byte to 2 chars
            val (c0, c1,  _,  _) = encodeGroup(bin(3 * i))
            encoded(4 * i + 0) = c0
            encoded(4 * i + 1) = c1
            encoded(4 * i + 2) = '='
            encoded(4 * i + 3) = '='
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

  /** Encodes a 6-bit value to a character */
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

    val decodedLen = encodedLen % 4 match {
      case 0 => encodedLen / 4 * 3
      case 3 => encodedLen / 4 * 3 + 2
      case 2 => encodedLen / 4 * 3 + 1
      case _ => throw new IllegalArgumentException(s"illegal Base 64 encoded length: ${
          text.length} (${text.truncate(100)})")
    }
    val decoded = new Array[Byte](decodedLen) // preallocate

    def decodeGroup(c0: Int, c1: Int = 0, c2: Int = 0, c3: Int = 0) = (
      (c0 << 2 | c1 >> 4).toByte,
      ((c1 & 0x0f) << 4 | c2 >> 2).toByte,
      ((c2 & 0x03) << 6 | c3).toByte,
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
          case k if (k >= 4) => // 4 chars to 3 bytes
            val (b0, b1, b2) = decodeGroup(decodeNext, decodeNext, decodeNext, decodeNext)
            decoded(j + 0) = b0
            decoded(j + 1) = b1
            decoded(j + 2) = b2

          case 3 => // 3 chars to 2 bytes
            val (b0, b1,  _) = decodeGroup(decodeNext, decodeNext, decodeNext)
            decoded(j + 0) = b0
            decoded(j + 1) = b1

          case 2 => // 2 chars to 1 byte
            val (b0, b1,  _) = decodeGroup(decodeNext, decodeNext)
            decoded(j + 0) = b0
        }
        loop(i2, j + 3)
      }
    }
    loop(0, 0)

    decoded
  }

  /** Decodes a character to a 6-bit value */
  def decode(ch: Char): Int
}

/** Base 64 Encoding (RFC 4648) */
object Base64 extends Base64:
  /** RFC 4648 Table 1 */
  override def encode(value: Int) = value match
    case value if value < 0 => throw new MatchError(value)
    case value if value < 26 => ('A' + value).toByte
    case value if value < 52 => ('a' + (value - 26)).toByte
    case value if value < 62 => ('0' + (value - 52)).toByte
    case 62 => '+'
    case 63 => '/'

  /** Reverse table */
  override def decode(ch: Char) = ch match
    case ch if ch >= 'A' && ch <= 'Z' => ch - 'A'
    case ch if ch >= 'a' && ch <= 'z' => ch - 'a' + 26
    case ch if ch >= '0' && ch <= '9' => ch - '0' + 52
    case '+' => 62
    case '/' => 63

  /** URL and Filename safe */
  object UrlSafe extends Base64:
    /** RFC 4648 Table 2 */
    override def encode(value: Int) = value match
      case value if value < 0 => throw new MatchError(value)
      case value if value < 26 => ('A' + value).toByte
      case value if value < 52 => ('a' + (value - 26)).toByte
      case value if value < 62 => ('0' + (value - 52)).toByte
      case 62 => '-' // minus
      case 63 => '_' // underline

    /** Reverse table */
    override def decode(ch: Char) = ch match
      case ch if ch >= 'A' && ch <= 'Z' => ch - 'A'
      case ch if ch >= 'a' && ch <= 'z' => ch - 'a' + 26
      case ch if ch >= '0' && ch <= '9' => ch - '0' + 52
      case '-' => 62 // minus
      case '_' => 63 // underline
