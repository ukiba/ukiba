package jp.ukiba.koneko
package ko_ffmpeg

import ko_fs2.io.file.KoFiles

import fs2.io.file.{Path, Files}
import org.typelevel.log4cats.Logger
import cats.effect.{Sync, Resource}
import cats.syntax.all.*

// adapted from jip camstream
class FFmpegProcessHelper[F[_]: Files](using F: Sync[F], log: Logger[F]):
  def reencode(outFile: Path, inFile: Path,
      videoCodec: Option[String] = None, // copy / libx264 / libxvid / mpeg4 / ffv1 / ...
      audioCodec: Option[String] = None, // copy / libopus / libvorbis / aac / libmp3lame / ac3 ...
      audioBitRate: Option[String] = None, // 96k / ... Consult https://wiki.xiph.org/Opus_Recommended_Settings
  ): F[String] =
    for
      _ <- KoFiles.createParentIfNotExists(outFile)

      command = Seq("ffmpeg",
          "-hide_banner", "-loglevel", "error", // quiet
          "-i", inFile.absolute.toString) ++
          videoCodec.map(Seq("-c:v", _)).getOrElse(Nil) ++
          audioCodec.map(Seq("-c:a", _)).getOrElse(Nil) ++
          audioBitRate.map(Seq("-b:a", _)).getOrElse(Nil) ++
          Seq(outFile.absolute.toString)

      runResult <- ProcessHelper().run(command*)
    yield runResult.stdout
