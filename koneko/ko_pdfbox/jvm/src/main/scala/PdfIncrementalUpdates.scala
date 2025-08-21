package jp.ukiba.koneko
package ko_pdfbox

import fs2.io.file.{FileHandle, Files, Flags, Path}
import cats.effect.Sync
import cats.syntax.all.*

class PdfIncrementalUpdates[F[_]: Files: Sync]:
  inline def F = Sync[F]
  inline def files = Files[F]
  import PdfIncrementalUpdates.PdfException

  def lastRangeMaxSize = 1024

  def parse(path: Path): F[Unit] =
    Files[F].open(path, Flags.Read).use: fh =>
      parse(fh)

  def parse(fh: FileHandle[F]): F[Unit] =
    for
      size <- fh.size

      // bail out when the size is too small
      // TODO find out the minimum size
      //  9 %PDF-x.y EOL
      //  5 xref EOL
      // 12 trailer <<>> EOL
      // 10 startxref EOL
      //  2 z EOL
      //  5 %%EOF
      _ <- if size < 40 then F.raiseError(PdfException(s"PDF size is too small: $size")) else F.unit

      lastRangeSize = if size > lastRangeMaxSize then lastRangeMaxSize else size.toInt
      lastRange <- fh.read(lastRangeSize, size - lastRangeSize)
    yield ()

object PdfIncrementalUpdates:
  class PdfException(message: String) extends Exception(message) // TODO ExceptionBase


import cats.effect.{IO, IOApp, ExitCode}

object PdfIncrementalUpdatesApp extends IOApp:
  override def run(args: List[String]): IO[ExitCode] =
    for
      _ <- PdfIncrementalUpdates[IO].parse(Path(args(0)))
    yield ExitCode.Success
