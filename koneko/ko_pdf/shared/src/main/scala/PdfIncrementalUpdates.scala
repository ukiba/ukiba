package jp.ukiba.koneko
package ko_pdf

import internal.*  // TODO get rid of this

import fs2.io.file.{FileHandle, Files, Flags, Path}
import fs2.Chunk
import cats.effect.Sync
import cats.syntax.all.*
import cats.ApplicativeThrow

class PdfIncrementalUpdates[F[_]: Files: Sync]:
  inline def F = Sync[F]
  inline def files = Files[F]
  import PdfIncrementalUpdates.*

  def lastRangeMaxSize = 8192 // heuristic of 1024 is common, but be safe

  def parse(path: Path): F[Unit] =
    Files[F].open(path, Flags.Read).use: fh =>
      parse(fh)

  def parse(fh: FileHandle[F]): F[Unit] =
    for
      size <- fh.size
      ret <- parse(fh, size)
    yield ret

  // FIXME the followings probably belong to a PdfFile class
  def parse(fh: FileHandle[F], size: Long): F[Unit] =
    for
      // bail out when the size is too small
      // TODO find out the minimum size
      //  9 %PDF-x.y EOL
      //  5 xref EOL
      // 12 trailer <<>> EOL
      // 10 startxref EOL
      //  2 z EOL
      //  5 %%EOF
      _ <- if size < 40 then raisePdfException(f"PDF size is too small: $size%,d bytes") else F.unit

      lastRangeSize = if size > lastRangeMaxSize then lastRangeMaxSize else size.toInt
      lastRange <- fh.readFully(lastRangeSize, size - lastRangeSize)

      remaining <- lastRange.dropRightWhile(_.toChar.isWhitespace).pure
      remaining <-
        if remaining.size < 5 then
          raisePdfException(f"Non whitespace text is not found in the last $lastRangeMaxSize%,d bytes")
        else
          val eofMarker = remaining.takeRight(5).toArray.parseString
          if !eofMarker.equalsIgnoreCase("%%EOF") then
            raisePdfException(f"%%%%EOF is not found in the last $lastRangeMaxSize%,d bytes")
          else
            remaining.dropRight(5).pure

      remaining <- remaining.dropRightWhile(_.toChar.isWhitespace).pure
      (xrefOffset, remaining) <-
        val numDigits = remaining.countRight(ch => ch >= '0' && ch <= '9')
        if numDigits > 0 then
          (remaining.takeRight(numDigits).toArray.parseString.toInt, remaining.dropRight(numDigits)).pure
        else
          raisePdfException(f"xrefOffset is not found")
      _ = println(f"xrefOffset = $xrefOffset%,d")

      remaining <- remaining.dropRightWhile(_.toChar.isWhitespace).pure
      remaining <-
        if remaining.size < 9 then
          raisePdfException(f"startxref is not found in the last $lastRangeMaxSize%,d bytes")
        else
          val startxref = remaining.takeRight(9).toArray.parseString
          if !startxref.equalsIgnoreCase("startxref") then
            raisePdfException(f"startxref is not found in the last $lastRangeMaxSize%,d bytes")
          else
            remaining.dropRight(9).pure

    yield ()

object PdfIncrementalUpdates:
  class PdfException(message: String) extends ExceptionBase(message)

  // this lets raising the exception to be concise
  def raisePdfException[F[_]: ApplicativeThrow, A](message: String): F[A] =
    inline def F = ApplicativeThrow[F]
    F.raiseError(PdfException(message))

import cats.effect.{IO, IOApp, ExitCode}

object PdfIncrementalUpdatesApp extends IOApp:
  override def run(args: List[String]): IO[ExitCode] =
    for
      _ <- PdfIncrementalUpdates[IO].parse(Path(args(0)))
    yield ExitCode.Success
