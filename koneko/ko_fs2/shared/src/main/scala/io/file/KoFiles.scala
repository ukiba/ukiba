package jp.ukiba.koneko
package ko_fs2
package io
package file

import fs2.io.file.{Files, Path, WriteCursor}
import fs2.text
import fs2.{Stream, Chunk, Pull}
import fs2.RaiseThrowable.fromApplicativeError
import cats.effect.std.AtomicCell
import cats.effect.kernel.{Concurrent, Resource}
import cats.Monad // FlatMap and Applicative (pure)
import cats.syntax.all.*

import java.nio.charset.{Charset, StandardCharsets}, StandardCharsets.UTF_8

object KoFiles:
  /** @return true when created */
  def createParentIfNotExists[F[_]: Files: Monad](path: Path): F[Boolean] = path.parent match
    case Some(parent) =>
      for
        exists <- Files[F].exists(parent)
        ret <- if !exists then Files[F].createDirectories(parent).as(true) else false.pure[F]
      yield ret

    case None => false.pure[F]

  /** A wrapper of WriteCursor that keeps the updated offset */
  class Appender[F[_]: Concurrent](cell: AtomicCell[F, WriteCursor[F]]):
    def write(bytes: Chunk[Byte]): F[WriteCursor[F]] =
      cell.evalUpdateAndGet: writeCursor =>
        writeCursor.write(bytes)

    def write(bytes: Array[Byte]): F[WriteCursor[F]] =
        write(Chunk.array(bytes))
    def write(bytes: Array[Byte], offset: Int, length: Int): F[WriteCursor[F]] =
        write(Chunk.array(bytes, offset, length))

    def write(string: String, charset: Charset = UTF_8): F[WriteCursor[F]] =
      val byteStream: Stream[F, Byte] = Stream.emits(string).through(text.char2string.andThen(text.encode(charset)))
      cell.evalUpdateAndGet: writeCursor =>
        val resultPull: Pull[F, WriteCursor[F], Unit] = writeCursor.writeAll(byteStream).flatMap: result =>
          Pull.output1(result) // replace the output with the result
        resultPull.stream.compile.onlyOrError

  object Appender:
    def of[F[_]: Concurrent](writeCursor: WriteCursor[F]): F[Appender[F]] =
      for
        cell <- AtomicCell[F].of(writeCursor)
      yield Appender(cell)

    def of[F[_]: Concurrent](writeCursor: Resource[F, WriteCursor[F]]): Resource[F, Appender[F]] =
      for
        writeCursor <- writeCursor
        appender <- Resource.eval(of(writeCursor))
      yield appender
