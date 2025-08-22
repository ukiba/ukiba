package jp.ukiba.koneko
package ko_pdf

import fs2.io.file.{FileHandle, Files, Flags, Path}
import fs2.Chunk
import cats.effect.Sync
import cats.syntax.all.*

import scala.annotation.tailrec
import java.io.IOException
import java.nio.charset.StandardCharsets.UTF_8

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
      _ <- if size < 40 then F.raiseError(PdfException(f"PDF size is too small: $size%,d bytes")) else F.unit

      lastRangeSize = if size > lastRangeMaxSize then lastRangeMaxSize else size.toInt
      lastRange <- fh.readFully(lastRangeSize, size - lastRangeSize)

      remaining <- lastRange.dropRightWhile(_.toChar.isWhitespace).pure
      remaining <-
        if remaining.size < 5 then
          F.raiseError(PdfException(f"Non whitespace text is not found in the last $lastRangeMaxSize%,d bytes"))
        else
          val eofMarker = String(remaining.takeRight(5).toArray, UTF_8)  // TODO use ko_scala extension method
          if !eofMarker.equalsIgnoreCase("%%EOF") then
            F.raiseError(PdfException(f"%%EOF is not found in the last $lastRangeMaxSize%,d bytes"))
          else
            remaining.dropRight(5).pure

      remaining <- remaining.dropRightWhile(_.toChar.isWhitespace).pure
      (xrefOffset, remaining) <-
        val numDigits = remaining.countRight(ch => ch >= '0' && ch <= '9')
        if numDigits > 0 then
          (String(remaining.takeRight(numDigits).toArray, UTF_8).toInt, remaining.dropRight(numDigits)).pure
        else
          F.raiseError(PdfException(f"xrefOffset is not found"))
      _ = println(f"xrefOffset = $xrefOffset%,d")

      remaining <- remaining.dropRightWhile(_.toChar.isWhitespace).pure
      remaining <-
        if remaining.size < 9 then
          F.raiseError(PdfException(f"startxref is not found in the last $lastRangeMaxSize%,d bytes"))
        else
          val startxref = String(remaining.takeRight(9).toArray, UTF_8)  // TODO use ko_scala extension method
          if !startxref.equalsIgnoreCase("startxref") then
            F.raiseError(PdfException(f"startxref is not found in the last $lastRangeMaxSize%,d bytes"))
          else
            remaining.dropRight(9).pure

    yield ()

object PdfIncrementalUpdates:
  class PdfException(message: String) extends Exception(message) // TODO ExceptionBase

  // FIXME the followings do not belong here
  extension [F[_]: Sync](fh: FileHandle[F])
    inline def F = Sync[F]

    def readFully(numBytes: Int, offset: Long): F[Chunk[Byte]] =
      def loop(accum: Chunk[Byte], remaining: Int, offset: Long, numEmptyReads: Int = 0): F[Chunk[Byte]] =
        if remaining == 0 then
          accum.pure

        else if remaining > 0 then
          for
            opt <- fh.read(remaining, offset)
            ret <- opt match
              case Some(bytes) =>
                if bytes.nonEmpty then
                  loop(accum ++ bytes, remaining - bytes.size, offset + bytes.size, 0)
                else
                  val nextNumEmptyReads = numEmptyReads + 1
                  // TODO: warm when nextNumEmptyReads is divisible by, e.g., 10
                  loop(accum, remaining, offset, numEmptyReads + 1)

              case None =>
                F.raiseError(IOException(f"read only ${accum.size}%,d / $numBytes%,d bytes"))
          yield ret

        else
          F.raiseError(IOException(f"read remaining = $remaining%,d / $numBytes%,d bytes"))

      loop(Chunk.empty, numBytes, offset)

  // FIXME the followings do not belong here
  extension [O](chunk: Chunk[O])
    def countRight(pred: O => Boolean): Int =
      @tailrec def loop(count: Int, lastIndex: Int): Int =
        if (lastIndex >= 0 && pred(chunk(lastIndex)))
          loop(count + 1, lastIndex - 1)
        else
          count
      loop(0, chunk.size - 1)

    def dropRightWhile(pred: O => Boolean): Chunk[O] = chunk.dropRight(chunk.countRight(pred))

import cats.effect.{IO, IOApp, ExitCode}

object PdfIncrementalUpdatesApp extends IOApp:
  override def run(args: List[String]): IO[ExitCode] =
    for
      _ <- PdfIncrementalUpdates[IO].parse(Path(args(0)))
    yield ExitCode.Success
