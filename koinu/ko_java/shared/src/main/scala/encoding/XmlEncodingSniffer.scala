package jp.ukiba.koinu.ko_java
package encoding

import java.nio.charset.{Charset, StandardCharsets, UnsupportedCharsetException},
    StandardCharsets.{UTF_8, UTF_16BE, UTF_16LE, UTF_32BE, UTF_32LE}

object XmlEncodingSniffer:
  // FIXME
  def extractEncoding(bytes: Array[Byte], maxScanBytes: Int = 4096): Option[String] = None

/*
  /*
    The class name contains `Xml` rather than `XML` because
    [Acronyms should be treated as normal words](https://docs.scala-lang.org/style/naming-conventions.html)
  */

  /** Heuristic: returns true if the prefix looks like an XML document. */
  def isXml(bytes: Array[Byte], maxScanBytes: Int = 4096): Boolean =
    val (charset, numBomBytes) = autodetect(bytes).getOrElse((UTF_8, 0))
    val cur = AsciiCursor(bytes, charset, numBomBytes, maxScanBytes)

    // Skip XML whitespace (ASCII only) before first markup.
    cur.skipXmlWhitespace()

    if !cur.hasMore then return false
    if cur.peekChar() != '<' then return false

    // Accept:
    //   <?xml ...?>
    //   <!DOCTYPE ...>
    //   <!-- ... -->
    //   <Name ...>
    cur.nextChar() // '<'

    if !cur.hasMore then return false
    cur.peekChar() match
      case '?' =>
        // Strictly accept only XML declaration as a strong signal.
        // (XML may start with a PI other than xml, but this keeps false-positives down.)
        cur.nextChar()
        cur.tryConsumeLiteral("xml")
      case '!' =>
        cur.nextChar()
        cur.tryConsumeLiteral("--") || cur.tryConsumeLiteral("DOCTYPE")
      case c =>
        isAsciiNameStart(c)

  /**
   * Returns the encoding name for an XML document, using only in-band information:
   * 1) `encoding="..."` in XML declaration, else
   * 2) BOM-derived (UTF-8/UTF-16/UCS-4 families), else
   * 3) UTF-8 default (only when the bytes look like XML and no external encoding info exists).
   */
  def extractEncoding(bytes: Array[Byte], maxScanBytes: Int = 4096): Option[String] =
    val (charset, numBomBytes) = autodetect(bytes).getOrElse((UTF_8, 0))

    // 1) Try declared encoding in XML declaration.
    extractDeclaredEncoding(bytes, charset, numBomBytes, maxScanBytes) match
      case some @ Some(_) => some
      case None =>
        // 2) BOM-derived encoding (if any).
        det.bomEncodingName
          .orElse:
            // 3) Default to UTF-8 only if it looks like XML.
            if isXml(bytes) then Some("UTF-8") else None

  /** Extracts only the encoding from `<?xml ... encoding="..."?>` (no BOM/default fallback). */
  def extractDeclaredEncoding(bytes: Array[Byte], maxScanBytes: Int = 4096): Option[String] =
    val (charset, numBomBytes) = autodetect(bytes).getOrElse((UTF_8, 0))
    extractDeclaredEncoding(bytes, charset, numBomBytes, maxScanBytes)

  // -----------------------
  // Internal implementation
  // -----------------------

  private def extractDeclaredEncoding(bytes: Array[Byte], charset: Charset, numBomBytes: Int, maxScanBytes: Int): Option[String] =
    val cur = AsciiCursor(bytes, charset, numBomBytes, maxScanBytes)

    // XMLDecl must start immediately (after BOM), not after whitespace.
    if !cur.tryConsumeLiteral("<?xml") then return None

    // Parse pseudo-attributes until '?>'
    while cur.hasMore do
      cur.skipXmlWhitespace()

      if cur.tryConsumeLiteral("?>") then return None // end of declaration, no encoding found

      val name = cur.readAsciiName()
      if name.isEmpty then
        // Not a well-formed decl; give up (sniffer, not a validator)
        return None

      cur.skipXmlWhitespace()
      if cur.peekCharOr0() != '=' then
        // XMLDecl requires '=' for pseudo attributes; treat as failure
        return None
      cur.nextChar() // '='
      cur.skipXmlWhitespace()

      val quote = cur.peekCharOr0()
      if quote != '"' && quote != '\'' then return None
      cur.nextChar()

      if name == "encoding" then
        val enc = cur.readEncNameUntil(quote)
        // consume closing quote if present
        if cur.peekCharOr0() == quote then cur.nextChar()
        return if enc.nonEmpty then Some(enc) else None
      else
        // Skip value
        cur.skipUntilChar(quote)
        if cur.peekCharOr0() == quote then cur.nextChar()

    None
*/

  private def isAsciiNameStart(c: Char): Boolean =
    (c >= 'A' && c <= 'Z') ||
    (c >= 'a' && c <= 'z') ||
     c == '_' ||
     c == ':'

  // [Appendix F](https://www.w3.org/TR/xml/#sec-guessing-no-ext-info)
  // Autodetection of Character Encodings (Non-Normative)
  def autodetect(bytes: Array[Byte]): Option[(charset: Charset, numBomBytes: Int)] =
    inline def byteAt(i: Int): Int = if i >= 0 && i < bytes.length then bytes(i) & 0xff else -1
    val b0 = byteAt(0)
    val b1 = byteAt(1)
    val b2 = byteAt(2)
    val b3 = byteAt(3)

    // With BOM
    // Ucs4_2143 and Ucs4_3412 are described as "unusual" in the spec, and not supported by Java
    if      b0 == 0xEF && b1 == 0xBB && b2 == 0xBF               then Some((UTF_8   , 3))
    else if b0 == 0x00 && b1 == 0x00 && b2 == 0xFE && b3 == 0xFF then Some((UTF_32BE, 4)) // Ucs4_1234
    else if b0 == 0xFF && b1 == 0xFE && b2 == 0x00 && b3 == 0x00 then Some((UTF_32LE, 4)) // Ucs4_4321
    else if b0 == 0x00 && b1 == 0x00 && b2 == 0xFF && b3 == 0xFE then throw UnsupportedCharsetException("Ucs4_2143")
    else if b0 == 0xFE && b1 == 0xFF && b2 == 0x00 && b3 == 0x00 then throw UnsupportedCharsetException("Ucs4_3412")
    else if b0 == 0xFE && b1 == 0xFF                             then Some((UTF_16BE, 2))
    else if b0 == 0xFF && b1 == 0xFE                             then Some((UTF_16LE, 2))
    // Without BOM
    else if b0 == 0x00 && b1 == 0x00 && b2 == 0x00 && b3 == 0x3C then Some((UTF_32BE, 0)) // Ucs4_1234
    else if b0 == 0x3C && b1 == 0x00 && b2 == 0x00 && b3 == 0x00 then Some((UTF_32LE, 0)) // Ucs4_4321
    else if b0 == 0x00 && b1 == 0x00 && b2 == 0x3C && b3 == 0x00 then throw UnsupportedCharsetException("Ucs4_2143")
    else if b0 == 0x00 && b1 == 0x3C && b2 == 0x00 && b3 == 0x00 then throw UnsupportedCharsetException("Ucs4_3412")
    else if b0 == 0x00 && b1 == 0x3C && b2 == 0x00 && b3 == 0x3F then Some((UTF_16BE, 0))
    else if b0 == 0x3C && b1 == 0x00 && b2 == 0x3F && b3 == 0x00 then Some((UTF_16LE, 0))
    // the default
    else                                                              None

  class AsciiCursor(bytes: Array[Byte], charset: Charset, offset: Int, maxScanBytes: Int):
    private var pos = offset

    private val limit: Int = Math.min(bytes.length, Math.max(0, maxScanBytes))

    def hasMore: Boolean = pos < limit
