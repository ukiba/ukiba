package jp.ukiba.koneko
package ko_fs2
package io
package file
package syntax

import fs2.io.file.{Path, Files, Flags, WriteCursor}
import fs2.{Stream, Pipe, Pull, Chunk}
import cats.effect.kernel.{MonadCancel, Concurrent, Resource}
import cats.syntax.all.*

import scala.annotation.targetName

trait FilesSyntax:
  opaque type CreateParentIfNotExists[F[_]] = Files[F]

  extension [F[_]: Files](files: Files[F])
    def createParentIfNotExists: CreateParentIfNotExists[F] = files

    /** A wrapper of WriteCursor that keeps the updated offset */
    def appender(path: Path)(using Concurrent[F]): Resource[F, KoFiles.Appender[F]] =
      KoFiles.Appender.of(writeCursor(path))

    // fs2-3.11.0: writeCursor is missing the signature without flags
    def writeCursor(path: Path): Resource[F, WriteCursor[F]] =
      files.writeCursor(path, Flags.Write)

  extension [F[_]: Files](files: CreateParentIfNotExists[F])
    @targetName("CreateParentIfNotExists.appender")
    def appender(path: Path)(using Concurrent[F]): Resource[F, KoFiles.Appender[F]] =
      Resource.eval(KoFiles.createParentIfNotExists(path)).flatMap: _ =>
        KoFiles.Appender.of(writeCursor(path))

    def writeAll(path: Path, flags: Flags = Flags.Write)(using MonadCancel[F, ?]): Pipe[F, Byte, Nothing] =
      in =>
        Stream.resource(Resource.eval(KoFiles.createParentIfNotExists(path))).flatMap: _ =>
          Files[F].writeAll(path)(in)

    def writeCursor(path: Path, flags: Flags = Flags.Write)(using MonadCancel[F, ?]): Resource[F, WriteCursor[F]] =
      Resource.eval(KoFiles.createParentIfNotExists(path)).flatMap: _ =>
        Files[F].writeCursor(path, flags)

    // TODO writeRotate

    def writeUtf8(path: Path, flags: Flags = Flags.Write)(using MonadCancel[F, ?]): Pipe[F, String, Nothing] =
      in =>
        Stream.resource(Resource.eval(KoFiles.createParentIfNotExists(path))).flatMap: _ =>
          Files[F].writeUtf8(path)(in)

    def writeUtf8Lines(path: Path, flags: Flags = Flags.Write)(using MonadCancel[F, ?]): Pipe[F, String, Nothing] =
      in =>
        Stream.resource(Resource.eval(KoFiles.createParentIfNotExists(path))).flatMap: _ =>
          Files[F].writeUtf8Lines(path)(in)
