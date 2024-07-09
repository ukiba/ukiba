package jp.ukiba.koneko
package ko_ffmpeg

import ko_fs2.io.file.KoFiles

import fs2.io.file.Path
import org.typelevel.log4cats.Logger
import cats.effect.{Sync, Resource}
import cats.syntax.all.*

class FFprobeProcessHelper[F[_]](using F: Sync[F], log: Logger[F]):
  def getDuration(inFile: Path): F[Iterator[Double]] =
    for
      command <- F.delay:
        Seq("ffprobe",
            "-hide_banner",
            "-show_format",
            "-i", inFile.absolute.toString)

      runResult <- ProcessHelper().run(command*)

      durationPrefix = "duration="
      durations = runResult.stdoutLines().flatMap: line =>
        if line.startsWith(durationPrefix) then
          try
            Some(line.drop(durationPrefix.length).toDouble)
          catch
            case ex: NumberFormatException =>
              ex.printStackTrace // TODO log.error
              None
        else
          None

    yield durations
