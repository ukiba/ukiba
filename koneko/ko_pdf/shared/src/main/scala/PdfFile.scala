package jp.ukiba.koneko
package ko_pdf

import internal.*  // TODO get rid of this

import fs2.io.file.{FileHandle, Files, Flags, Path}
import fs2.Chunk
import cats.effect.Sync
import cats.syntax.all.*
import cats.ApplicativeThrow

import scala.annotation.tailrec

class PdfFile[F[_]: Files: Sync]:
  inline def F = Sync[F]
  inline def files = Files[F]
  import PdfFile.*

  def firstChunkMaxSize = 1024
  def lastRangeMaxSize = 2048 // heuristic of 1024 is common, but be safe

  val versionMarker = Array[Byte]('%', 'P', 'D', 'F', '-')

  def parse(path: Path): F[Unit] =
    Files[F].open(path, Flags.Read).use: fh =>
      parse(fh)

  def parse(fh: FileHandle[F]): F[Unit] =
    for
      size <- fh.size
      ret <- parse(fh, size)
    yield ret

  def parse(fh: FileHandle[F], size: Long): F[Unit] =
    for
      // bail out with a descriptive error message early
      // the smallest possible PDF should be
      // 1. 294 bytes ([spec compliant](https://pdfa.org/the-smallest-possible-valid-pdf/))
      // 2. 36 bytes ([with hacks](https://stackoverflow.com/a/66574838))
      _ <- if size < 36 then raiseParseException(f"PDF file is too small: $size%,d bytes") else F.unit

      pdfVersion <- parseHeader(fh, size)
      _ = println(s"pdfVersion = $pdfVersion")

      xref <- parseXRefOffset(fh, size)
    yield ()

  /** Parses startxref, the offset, and "%%EOF" in backward. */
  def parseHeader(fh: FileHandle[F], size: Long): F[PdfHeader] =
    for
      firstChunkSize = if size > firstChunkMaxSize then firstChunkMaxSize else size.toInt
      firstChunk <- fh.readFully(firstChunkSize, 0)

      // junk data before the %PDF- version header
      // accounted for roughly 50% of errors
      // https://eliot-jones.com/2025/8/pdf-parsing-xref
      versionOffsetOpt = firstChunk.indexOfOpt(versionMarker)
      versionOffset <- versionOffsetOpt match
        case Some(versionOffset) => versionOffset.pure
        case None                => raiseParseException(s"""PDF file must starts with "%PDF-": ${
            firstChunk.dropWhile(_.toChar.isWhitespace).take(5).toArray.parseString}""")

      version <-
        val offset = versionOffset + versionMarker.size

        // version number is always a single digit
        if firstChunk(offset    ) >= '1' && firstChunk(offset    ) <= '9' && // there is not PDF version 0.x
           firstChunk(offset + 1) == '.' &&
           firstChunk(offset + 2) >= '0' && firstChunk(offset + 2) <= '9' then
          PdfVersion(firstChunk(offset) - '0', firstChunk(offset + 2) - '0', offset, 3).pure
        else
          raiseParseException(s"Illegal PDF file version: ${firstChunk.drop(offset).take(3).toArray.parseString}")

      /*
        7.5.2 File header

        If a PDF file contains binary data, as most do (see 7.2, "Lexical conventions"), the header line shall be immediately followed by a comment line containing at least four binary characters–that is, characters whose codes are 128 or greater. This ensures proper behaviour of file transfer applications that inspect data near the beginning of a file to determine whether to treat the file’s contents as text or as binary.
      */
      // it is usually "%âãÏÓ"
      binaryComment =
        firstChunk.indexWhereOpt(!_.toChar.isWhitespace, version.offset + version.len).flatMap: offset =>
          Option.when(firstChunk(offset) == '%')(offset).flatMap: offset =>
            @tailrec def loop(index: Int): Int =
              if index < firstChunk.size && (firstChunk(index) & 0xff) >= 128 then
                loop(index + 1)
              else
                index
            val len = loop(offset + 1) - offset
            Option.when(len >= 1 + 4):
              PdfBinaryComment(offset, len)

      // there are only comments and whitespaces
      // between the version line and the first indirect object,
      // which can indicate the PDF is linearized

    yield PdfHeader(version, binaryComment)

  def isLinearized(pdfVersion: (Int, Int)): F[Boolean] =
    // PDF version 1.2 or later
    // https://pdf-issues.pdfa.org/32000-2-2020/clauseAnnexF.html
    def checkPdfVersion: Boolean = pdfVersion match
      case (major, minor) if major >= 2 || minor >= 2 => true
      case _                                          => false

    checkPdfVersion.pure // FIXME

  /** Parses startxref, the offset, and "%%EOF" in backward. */
  def parseXRefOffset(fh: FileHandle[F], size: Long): F[XRefOffsetParsed] =
    for
      lastRangeSize = if size > lastRangeMaxSize then lastRangeMaxSize else size.toInt
      lastRangeOffset = size - lastRangeSize
      lastRange <- fh.readFully(lastRangeSize, size - lastRangeSize)

      remaining <- lastRange.dropRightWhile(_.toChar.isWhitespace).pure
      remaining <-
        if remaining.size < 5 then
          raiseParseException(f"Non whitespace text is not found in the last $lastRangeMaxSize%,d bytes")
        else
          val eofMarker = remaining.takeRight(5).toArray.parseString
          if !eofMarker.equalsIgnoreCase("%%EOF") then
            raiseParseException(f"%%%%EOF is not found in the last $lastRangeMaxSize%,d bytes")
          else
            remaining.dropRight(5).pure

      remaining <- remaining.dropRightWhile(_.toChar.isWhitespace).pure
      (xrefOffset, remaining) <-
        val numDigits = remaining.countRight(ch => ch >= '0' && ch <= '9')
        if numDigits > 0 then
          (remaining.takeRight(numDigits).toArray.parseString.toInt, remaining.dropRight(numDigits)).pure
        else
          raiseParseException(f"xrefOffset is not found")

      remaining <- remaining.dropRightWhile(_.toChar.isWhitespace).pure
      remaining <-
        if remaining.size < 9 then
          raiseParseException(f"startxref is not found in the last $lastRangeMaxSize%,d bytes")
        else
          val startxref = remaining.takeRight(9).toArray.parseString
          if !startxref.equalsIgnoreCase("startxref") then
            raiseParseException(f"startxref is not found in the last $lastRangeMaxSize%,d bytes")
          else
            remaining.dropRight(9).pure

    yield XRefOffsetParsed(xrefOffset, lastRangeOffset, remaining)

object PdfFile:
  case class PdfHeader(
    version: PdfVersion,
    binaryComment: Option[PdfBinaryComment],
    //linearized: Option[PdfLinearized],
  )
  case class PdfBinaryComment(offset: Int, len: Int)
  case class PdfVersion(major: Int, minor: Int, offset: Int, len: Int)

  case class XRefOffsetParsed(offset: Long, lastRangeOffset: Long, lastRangeRemaining: Chunk[Byte])

  class PdfParseException(message: String, cause: Throwable = null) extends ExceptionBase(message, cause)

  // this lets raising the exception to be concise
  def raiseParseException[F[_]: ApplicativeThrow, A](message: String): F[A] =
    ApplicativeThrow[F].raiseError(PdfParseException(message))

  def raiseParseExceptionWhen[F[_]: ApplicativeThrow, A](pred: => Boolean)(message: String): F[Unit] =
    if pred then raiseParseException(message) else ApplicativeThrow[F].unit
