package jp.ukiba.koneko
package ko_pdf

import internal.*  // TODO get rid of this

import fs2.Chunk

/** PDF Lexical conventions */
object PdfLex:
  // 7.2.3 Character set

  inline def isWhitespace(bt: Byte): Boolean = bt match
    case 0 | '\t' | '\n' | '\f' | '\r' | ' ' => true
    case _                                   => false

  inline def isDelimiter(bt: Byte): Boolean = bt match
    // delimit syntactic entities such as arrays, names, and comments
    case '(' | ')' | '<' | '>' | '[' | ']' | '/' | '%' => true 

    // additional delimiter characters within Type 4 PostScript calculator functions
    // case '{' | '}' =>

    case _ => false

  inline def isRegular(bt: Byte): Boolean = !isWhitespace(bt) && !isDelimiter(bt)

  def nextToken(buf: Chunk[Byte], offset: Int): Option[Token] =
    if offset < buf.size then
      buf(offset) match
        case '%' =>
          // The comment consists of all characters after the PERCENT SIGN and up to but not including the end-of-the-line marker
          buf.indexWhereOpt(bt => bt != '\r' || bt != '\n', offset + 1).flatMap: eol => // TODO warn when '\n'
            nextToken(buf, eol) // TODO should consume following '\n' when `\r'

        case bt if isWhitespace(bt) =>
          buf.indexWhereOpt(bt => !isWhitespace(bt), offset + 1).flatMap: next =>
            nextToken(buf, next)

        case bt if isDelimiter(bt) =>
          Some(Delimiter(bt, offset + 1))

        case _ =>
          Some(Text(Array.empty, offset)) // FIXME
    else
      None

  sealed trait Token:
    def end: Int
  case class Delimiter(byte : Byte       , end: Int) extends Token
  case class Text     (bytes: Array[Byte], end: Int) extends Token

  type PdfObject = PdfBoolean /*| PdfInteger | PdfReal | PdfString | PdfName |
                   PdfArray | PdfDict | PdfStream*/ | PdfNull

  type PdfBoolean = Boolean

  // C.2: Integer values (such as object numbers) can often be expressed within 32 bits.
  // but /Length of a stream is integer and there are PDF streams that is 2 GB or larger
  // 2GB or larger embedded files also exist
  type PdfInteger = Long

  // C.2: Modern computers often represent and process real numbers using IEEE Standard for Floating-Point Arithmetic (IEEE 754) single or double precision.
  // A real number shall not be present when an integer is expected.
  // Wherever a real number is expected, an integer may be used instead.
  type PdfReal    = Double
/*
  type PdfString  = 
  type PdfName    = 
  type PdfArray   = 
  type PdfDict    = 
  type PdfStream  = 
*/
  type PdfNull    = Null

  //inline val trueBytes = 

  def nextObject(buf: Chunk[Byte], offset: Int): Option[(PdfObject, Int)] =
    nextToken(buf, offset).map: token =>
      token match
        case Text(bytes, end) if bytes.sameElements(Array[Byte]('t', 'r', 'u', 'e'))  => (true , end)
        case Text(bytes, end) if bytes.sameElements(Array[Byte]('f', 'a', 'l', 's', 'e')) => (false, end)

        case Text(bytes, end) if (bytes(0) >= '0' && bytes(0) <= '9') || bytes(0) == '-' || bytes(0) == '+' =>
          (null, end) // FIXME

        case Text(bytes, end) if bytes.sameElements(Array[Byte]('n', 'u', 'l', 'l')) => (null, end)
