package jp.ukiba.koinu.ko_java
package encoding

import scala.collection.immutable
import java.nio.charset.{Charset, StandardCharsets}

/** Base64 Encoding (RFC 4648) */
trait Base64 {
  /**
   * Encodes an array of bytes into a text.
   * Each 3 bytes (24 bits) are encoded to 4 characters.
   * No new line characters are inserted.
   */
  def encode(bin: Array[Byte]): String = bin.sliding(3, 3).map { window => window.length match {
    // TODO don't produce window array
    case 3 => // 3 bytes
      Seq(
        (window(0) & 0xfc) >> 2, // 0xfc = 11111100
        (window(0) & 0x03) << 4 | (window(1) & 0xf0) >> 4,
        (window(1) & 0x0f) << 2 | (window(2) & 0xc0) >> 6, // 0xc0 = 11000000
        window(2) & 0x3f // 0x3f = 00111111
      ).map(encode)

    case 2 => // 2 bytes
      Seq(
        (window(0) & 0xfc) >> 2,
        (window(0) & 0x03) << 4 | (window(1) & 0xf0) >> 4,
        (window(1) & 0x0f) << 2
      ).map(encode) :+ '='

    case 1 => // 1 byte
      Seq(
        (window(0) & 0xfc) >> 2,
        (window(0) & 0x03) << 4
      ).map(encode) ++ Seq('=', '=')

  }}.flatten.mkString

  def encode(text: String, charset: Charset = StandardCharsets.UTF_8): String =
      encode(text.getBytes(charset))

  /** Encodes a 6-bit value to a character */
  def encode(value: Int): Char 

  /**
   * Decodes an encoded text.
   * Each 4 characters (24 bits) are decoded to 3 bytes.
   * New line characters are ignored.
   */
  def decode(text: String): Array[Byte] = text.filter(ch => ch != '\n' && ch != '\r').sliding(4, 4).map { window =>
    if (window.length != 4)
      throw IllegalArgumentException(s"text.length is not divisible by 4")

    if (window(3) != '=') { // 3 bytes
      val (o0, o1, o2, o3) = (decode(window(0)), decode(window(1)), decode(window(2)), decode(window(3)))
      Seq(
        (o0 << 2 | o1 >> 4).toByte,
        ((o1 & 0x0f) << 4 | o2 >> 2).toByte,
        ((o2 & 0x03) << 6 | o3).toByte
      )

    } else if (window(2) != '=') { // 2 bytes
      val (o0, o1, o2) = (decode(window(0)),decode(window(1)), decode(window(2)))
      Seq(
        (o0 << 2 | o1 >> 4).toByte,
        ((o1 & 0x0f) << 4 | o2 >> 2).toByte
      )

    } else { // 1 byte
      val (o0, o1) = (decode(window(0)), decode(window(1)))
      Seq(
        (o0 << 2 | o1 >> 4).toByte
      )
    }

  }.flatten.toArray

  /** Decodes a character to a 6-bit value */
  def decode(ch: Int): Int
}

/** Basic Base64 Encoding (RFC 4648) */
object Base64 extends Base64 {
  /** RFC 4648 Table 1 */
  def encode(value: Int) = value match {
    case value if (value < 0) => throw MatchError(value)
    case value if (value < 26) => ('A' + value).toChar
    case value if (value < 52) => ('a' + (value - 26)).toChar
    case value if (value < 62) => ('0' + (value - 52)).toChar
    case 62 => '+'
    case 63 => '/'
  }

  /** Reverse table */
  def decode(ch: Int) = ch match {
    case ch if (ch >= 'A' && ch <= 'Z') => ch - 'A'
    case ch if (ch >= 'a' && ch <= 'z') => ch - 'a' + 26
    case ch if (ch >= '0' && ch <= '9') => ch - '0' + 52
    case '+' => 62
    case '/' => 63
  }
}

/** URL Safe Base64 Encoding (RFC 4648) */
object Base64ForUrl extends Base64 {
  /** RFC 4648 Table 2 */
  def encode(value: Int) = value match {
    case value if (value < 0) => throw MatchError(value)
    case value if (value < 26) => ('A' + value).toChar
    case value if (value < 52) => ('a' + (value - 26)).toChar
    case value if (value < 62) => ('0' + (value - 52)).toChar
    case 62 => '-' // minus
    case 63 => '_' // underline
  }

  /** Reverse table */
  def decode(ch: Int) = ch match {
    case ch if (ch >= 'A' && ch <= 'Z') => ch - 'A'
    case ch if (ch >= 'a' && ch <= 'z') => ch - 'a' + 26
    case ch if (ch >= '0' && ch <= '9') => ch - '0' + 52
    case '-' => 62 // minus
    case '_' => 63 // underline
  }
}
