package jp.ukiba.koinu

import scala.annotation.tailrec
import java.nio.charset.{Charset, StandardCharsets}, StandardCharsets.{UTF_8, UTF_16}
import java.security.MessageDigest
import javax.crypto.spec.SecretKeySpec
import javax.crypto.Mac
import java.time.Instant

// hash example: "abc".utf8.sha2.sha256

package object ko_java:
  extension (str: String)
    def utf8Bytes : Array[Byte] = str.getBytes(UTF_8)
    def utf16Bytes: Array[Byte] = str.getBytes(UTF_16) // Java native encoding

    def countRight(pred: Char => Boolean): Int =
      @tailrec def loop(count: Int, lastIndex: Int): Int =
        if (lastIndex >= 0 && pred(str(lastIndex)))
          loop(count + 1, lastIndex - 1)
        else
          count
      loop(0, str.length - 1)

    def dropRightWhile(pred: Char => Boolean): String = str.dropRight(str.countRight(pred))

    /**
     * Truncates the string if its length is more than maxLen.
     * This appends the given suffix when truncated (the total length will be maxLen)
    */
    def truncateWith(maxLen: Int, suffix: String = "..."): String = {
      require(suffix.length <= maxLen)
      if (str.length <= maxLen)
        str
      else
        str.take(maxLen - suffix.length) + suffix
    }

    def parseHexBytes: Array[Byte] =
        // Use Short.parseShort because Byte is signed and
        // java.lang.Byte.parseByte("80", 16) results in
        // java.lang.NumberFormatException: Value out of range. Value:"80" Radix:16
        str.sliding(2, 2).map(java.lang.Short.parseShort(_, 16).toByte).toArray

    /** Removes the trailing zeros, and remove the point when no fractions remains */
    def numberWithoutZeroFractions: String = str.dropRightWhile(_ == '0').dropRightWhile(_ == '.')

  extension (bytes: Array[Byte])
    // needs to mask with 0xff on Scala.js 1.17

    /*
      Lower case is consistent with
      1. Unix tools (xxd, hexdump, sha256sum, Git object IDs, OpenSSL digests)
      2. RFC 5952 (IPv6): address MUST be represented in lowercase
      3. AWS Signature Version 4: Signature must be in lowercase

      However, the followings use the uppwe cases
      1. RFC 3986 (URI): should use uppercase hexadecimal digits for all percent-encodings
      2. RFC 4648 (Base16): upper case is used in the table
      3. RFC 8427 (DNS-in-JSON): Names that end in "HEX" ... in base16 encoding (hex with uppercase letters)
    */
    def toHexString             : String = bytes.map(bt => f"${bt & 0xff}%02x").mkString
    def toHexString(sep: String): String = bytes.map(bt => f"${bt & 0xff}%02x").mkString(sep)

    def toHexStringUpperCase             : String = bytes.map(bt => f"${bt & 0xff}%02X").mkString
    def toHexStringUpperCase(sep: String): String = bytes.map(bt => f"${bt & 0xff}%02X").mkString(sep)

  extension [A](ite: Iterable[A])
    def --(excludes: Iterable[A]): Iterable[A] = ite.filterNot(elem => excludes.exists(_ == elem))

  extension [A](seq: Seq[A])
    def --(excludes: Seq[A]): Seq[A] = seq.filterNot(elem => excludes.exists(_ == elem))

    def countRight(pred: A => Boolean): Int =
      @tailrec def loop(count: Int, lastIndex: Int): Int =
        if (lastIndex >= 0 && pred(seq(lastIndex)))
          loop(count + 1, lastIndex - 1)
        else
          count
      loop(0, seq.length - 1)

    def dropRightWhile(pred: A => Boolean): Seq[A] = seq.dropRight(seq.countRight(pred))

  // TODO move to KoCrypt in ko_cats_effect

  def hash(algo: String)(data: Array[Byte]): Array[Byte] = MessageDigest.getInstance(algo).digest(data)
  def hmac(algo: String)(key: Array[Byte])(data: Array[Byte]): Array[Byte] =
    val mac = Mac.getInstance(algo)
    mac.init(SecretKeySpec(key, algo))
    mac.doFinal(data)

  extension (bytes: Array[Byte])
    // https://docs.oracle.com/en/java/javase/21/docs/specs/security/standard-names.html#messagedigest-algorithms
    // MD2 is not used since it is hardly used
    inline def md5 : Array[Byte] = hash("MD5"  )(bytes) // MD5 is defined for compatibility

    // name as `sha1` rather-than `sha-1` like
    // https://github.com/typelevel/fs2/blob/main/core/jvm/src/main/scala/fs2/hash.scala
    // https://github.com/typelevel/fs2/blob/main/core/js/src/main/scala/fs2/hash.scala
    inline def sha1: Array[Byte] = hash("SHA-1")(bytes) // SHA-1 is widely used

    // SHA-2
    // NIST: SHA-224 will be disallowed after 2030-12-31 (SP 800-131A rev. 3)
    inline def sha256    : Array[Byte] = hash("SHA-256"    )(bytes)
    inline def sha384    : Array[Byte] = hash("SHA-384"    )(bytes)
    inline def sha512    : Array[Byte] = hash("SHA-512"    )(bytes)
    // NIST: SHA-512/224 will be disallowed after 2030-12-31 (SP 800-131A rev. 3)
    inline def sha512_256: Array[Byte] = hash("SHA-512/256")(bytes)

    // SHA-3 (Keccak)
    // NIST: SHA3-224 will be disallowed after 2030-12-31 (SP 800-131A rev. 3)
    inline def sha3_256: Array[Byte] = hash("SHA3-256")(bytes)
    inline def sha3_384: Array[Byte] = hash("SHA3-384")(bytes)
    inline def sha3_512: Array[Byte] = hash("SHA3-512")(bytes)

    inline def hmac = ArrayByteOps.Hmac(bytes)

  object ArrayByteOps:
    class Hmac(bytes: Array[Byte]) extends AnyVal:
      inline def sha1      (key: Array[Byte]): Array[Byte] = hmac("HmacSHA1"      )(key)(bytes)

      inline def sha256    (key: Array[Byte]): Array[Byte] = hmac("HmacSHA256"    )(key)(bytes)
      inline def sha384    (key: Array[Byte]): Array[Byte] = hmac("HmacSHA384"    )(key)(bytes)
      inline def sha512    (key: Array[Byte]): Array[Byte] = hmac("HmacSHA512"    )(key)(bytes)
      inline def sha512_256(key: Array[Byte]): Array[Byte] = hmac("HmacSHA512/256")(key)(bytes)

      inline def sha3_256  (key: Array[Byte]): Array[Byte] = hmac("HmacSHA3-256"  )(key)(bytes)
      inline def sha3_384  (key: Array[Byte]): Array[Byte] = hmac("HmacSHA3-384"  )(key)(bytes)
      inline def sha3_512  (key: Array[Byte]): Array[Byte] = hmac("HmacSHA3-512"  )(key)(bytes)

  object Charsets:
    // https://docs.oracle.com/en/java/javase/17/intl/supported-encodings.html
    inline def utf8  = UTF_8
    inline def utf16 = UTF_16

    def `windows-1252`  = Charset.forName("windows-1252")  // [HTML5](https://encoding.spec.whatwg.org/#names-and-labels) and modern email: ISO-8859-1 is actually [windows-1252](https://www.w3schools.com/charsets/ref_html_ansi.asp)
    def `windows-31j`   = Charset.forName("windows-31j")   // should be used instead of Shift_JIS
    def `EUC-JP`        = Charset.forName("EUC-JP")
    def `ISO-2022-JP-2` = Charset.forName("ISO-2022-JP-2") // includes JIS X 0212
    // ISO-2022-JP
    // x-euc-jp-linux
    // x-windows-iso2022jp

/*
  extension [A](ite: Iterable[A])
    def --(excludes: Iterable[A]): Iterable[A] = ite.filterNot(elem => excludes.exists(_ == elem))

  extension [A](seq: Seq[A])
    def --(excludes: Seq[A]): Seq[A] = seq.filterNot(elem => excludes.exists(_ == elem))
*/

/*
  there should be a wa to use <= and >=

  extension (instant: Instant)
    def isEqualOrAfter (other: Instant): Boolean = !instant.isBefore(other)
    def isEqualOrBefore(other: Instant): Boolean = !instant.isAfter (other)
*/
