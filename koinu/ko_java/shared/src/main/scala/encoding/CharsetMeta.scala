package jp.ukiba.koinu.ko_java
package encoding

import java.nio.charset.Charset

object CharsetMeta:
  def isIso2022(charset: Charset): Boolean = isIso2022(charset.name)

  /**
   * Decides if a given name is ISO-2022 family
   * @param name Canonical Name for java.nio API.
   *             [[java.nio.charset.Charset]] names are not case-sensitive
   * @see https://docs.oracle.com/en/java/javase/25/intl/supported-encodings.html
   */
  def isIso2022(name: String): Boolean =
    // ISO-2022-JP
    // ISO-2022-JP-2
    // ISO-2022-CN
    // ISO-2022-KR
    name.startsWithIgnoreCase("ISO-2022-") ||
    // x-ISO-2022-CN-CNS
    // x-ISO-2022-CN-GB
    name.startsWithIgnoreCase("x-ISO-2022-") ||
    // Windows specific
    name.equalsIgnoreCase("x-windows-iso2022jp") ||
    // ISO-2022-JP variants (excluding 50222 because it it not supported in Java)
    // https://learn.microsoft.com/en-us/windows/win32/intl/code-page-identifiers
    name.equalsIgnoreCase("x-windows-50220") ||
    name.equalsIgnoreCase("x-windows-50221")

  object cached:
    // CharsetDecoder properties
    final case class DecoderProp(maxCharsPerByte: Float, averageCharsPerByte: Float)

    // the key is the canonical name (forName("utf8") and StandardCharsets.UTF_8 should produce the same)
    final val decoderProps = CowCache.ofMapWithKeyTransform[Charset, String, DecoderProp](_.name): charset =>
      val dec = charset.newDecoder()
      DecoderProp(dec.maxCharsPerByte, dec.averageCharsPerByte)

    def decoderPropOf(charset: Charset): DecoderProp = decoderProps.of(charset)

    def maxCharsPerByteOf(charset: Charset): Float =
      decoderPropOf(charset).maxCharsPerByte

    def averageCharsPerByteOf(charset: Charset): Float =
      decoderPropOf(charset).averageCharsPerByte

    // CharsetEncoder properties
    final case class EncoderProp(maxBytesPerChar: Float, averageBytesPerChar: Float)

    // the key is the canonical name (forName("utf8") and StandardCharsets.UTF_8 should produce the same)
    final val encoderProps = CowCache.ofMapWithKeyTransform[Charset, String, EncoderProp](_.name): charset =>
      val enc = charset.newEncoder()
      EncoderProp(enc.maxBytesPerChar, enc.averageBytesPerChar)

    def encoderPropOf(charset: Charset): EncoderProp = encoderProps.of(charset)

    def maxBytesPerCharOf(charset: Charset): Float =
      encoderPropOf(charset).maxBytesPerChar

    def averageBytesPerCharOf(charset: Charset): Float =
      encoderPropOf(charset).averageBytesPerChar
