package jp.ukiba.koneko
package ko_pdf

import internal.*  // TODO get rid of this

import fs2.io.file.{FileHandle, Files, Flags, Path}
import fs2.Chunk
import cats.effect.Sync
import cats.syntax.all.*

class PdfIncrementalUpdates[F[_]: Files: Sync]:
  inline def F = Sync[F]
  inline def files = Files[F]

  def parse(path: Path): F[Unit] =
    PdfFile[F].parse(path)


import cats.effect.{IO, IOApp, ExitCode}

object PdfIncrementalUpdatesApp extends IOApp:
  import PdfFile.PdfParseException

  // ko_pdfJVM/runMain jp.ukiba.koneko.ko_pdf.PdfIncrementalUpdatesApp "/Users/kenichi/seiko-pades/pdf/thirdparty/ssol-2022-10/101_Adobe Acrobat Reader DC.pdf" /Users/kenichi/seiko-pades/pdf/evip5/sysart-2022-11-10/ESA+DTS+DTS.pdf
  override def run(args: List[String]): IO[ExitCode] =
    for
      _ <- args.parTraverse: arg =>
        PdfIncrementalUpdates[IO].parse(Path(arg)).adaptError:
          case ex: PdfParseException => PdfParseException(s"$arg: ${ex.getMessage}", ex)
    yield ExitCode.Success
