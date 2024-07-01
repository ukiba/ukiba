package jp.ukiba.koinu
package asn1

import scodec.bits.ByteVector

object der:
  trait DEREncoding[A] extends ASN1Encoding[A]

  // only definitive form
  def encodeLength(len: BigInt): ByteVector =
    if len < 0x80 then // short form
      ByteVector(len.toByte)

    else // long form
      encodeLengthLongForm(len)

  // specialized to Int
  def encodeLength(len: Int): ByteVector =
    if len < 0x80 then // short form
      ByteVector(len.toByte)

    else // long form
      encodeLengthLongForm(len)

  private def encodeLengthLongForm(len: BigInt): ByteVector =
    val lenContent = ByteVector.view(len.toByteArray) match
      case bytes if bytes(0) == 0 => bytes.drop(1) // remove the extra byte for the sign (length is unsigned)
      case bytes => bytes

    // bit 8 will be always 1
    // bit 7 must be 0 for future extension
    if lenContent.length > 0x3F then
      throw DERDecodeException(s"DER length: The value exceeds the maximum endodable value: ${lenContent.length}")

    val firstByte = (0x80 | lenContent.length).toByte
    firstByte +: lenContent

  given DEREncoding[Boolean] with
    def encode(value: Boolean): ByteVector =
      ByteVector(
        1, // length
        if value then 0xff else 0,
      )

    def decode(bin: ByteVector): (Boolean, ByteVector) =
      if bin.length < 2 then
        throw DERDecodeException(s"DER Boolean: Expecting 2 bytes but there are only ${bin.length}")

      else
        if bin(0) != 1 then
          throw DERDecodeException(s"DER Boolean: The length must be 1: ${bin(0)}")

        val value = bin(1) match
          case 0    => false
          case 0xff => true
          case bt => throw DERDecodeException(f"DER Boolean: Encoded byte must be 0x00 or 0xFF: 0x${bin(1)}%02X")

        (value, bin.drop(2))

  given DEREncoding[BigInt] with
    def encode(value: BigInt): ByteVector =
      // BigInteger.toByteArray returns two's complement
      // in big endian order (the most significant bit is in the first byte),
      // in the minimum number of bytes and italways includes the sign bit
      /*
        ITU-T X.690 Section 8.3.2 requires that when there are multiple bytes,
        the bits of the first byte and the most significant bit of the second byte

        (1) shall not all be zero (00000000 0XXXXXXX)

          For example,
          decimal 127 (hex 7F) will be binary          01111111
          rather than                  binary 00000000 01111111

        (2) shall not all be ones (11111111 1XXXXXXX)

          For example,
          decimal -128  will be binary          10000000
          rather than           binary 11111111 10000000
      */
      val content = value.toByteArray

      encodeLength(content.length) ++ ByteVector.view(content) // not performant https://github.com/scala/scala/blob/v2.13.14/src/library/scala/Array.scala#L150

    def decode(bin: ByteVector): (BigInt, ByteVector) = ???
      //val (len, bin) = decodeASN1Length(bin)

  class DERDecodeException(message: String, cause: Throwable = null) extends ASN1DecodeException(message, cause)
