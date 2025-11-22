package jp.ukiba.koneko
package ko_fs2
package io
package file

import fs2.io.file.FileHandle
import fs2.Chunk
import cats.effect.{Resource, Sync}
import cats.syntax.all.*

import java.io.IOException

trait KoRandomAccessRead[F[_]: Sync]:
  import KoRandomAccessRead.*

  /** @return The current size of the source. */
  def size: F[Long]

  /**
   * Reads at most `numBytes` starting at `offset` from the start of the source.
   * @return Fewer bytes than `numBytes` only when EOF is reached.
   */
  def read(numBytes: Int, offset: Long): F[Chunk[Byte]]

  // blockSize should be a few MB for S3
  def cached(blockSize: Int = 256 * 1024, maxBlocks: Int = 40): Cached[F] =
    Cached(this, blockSize, maxBlocks)

object KoRandomAccessRead:
  // FileHandle implementation

  def apply[F[_]: Sync](fh: Resource[F, FileHandle[F]]): Resource[F, KoRandomAccessRead[F]] =
    fh.map(FileHandleRead(_))

  class FileHandleRead[F[_]: Sync](fh: FileHandle[F]) extends KoRandomAccessRead[F]:
    inline def F = Sync[F]

    def size = fh.size

    def read(numBytes: Int, offset: Long) =
      def loop(accum: Chunk[Byte], remaining: Int, offset: Long): F[Chunk[Byte]] =
        if remaining == 0 then
          accum.pure

        else if remaining > 0 then
          for
            opt <- fh.read(remaining, offset)
            ret <- opt match
              case Some(bytes) => // bytes.nonEmpty is true only when numBytes == 0 (this will not be reached)
                loop(accum ++ bytes, remaining - bytes.size, offset + bytes.size)

              case None => // EOF
                accum.pure
          yield ret

        else
          F.raiseError(IOException(f"read remaining = $remaining%,d / $numBytes%,d bytes"))

      loop(Chunk.empty, numBytes, offset)

  // TODO mmap
  // TODO S3

  /** This class is not Concurrent */
  class Cached[F[_]: Sync](read: KoRandomAccessRead[F], blockSize: Int, maxBlocks: Int):
    inline def F = Sync[F]

    def size: F[Long] = read.size

    def read(numBytes: Int, offset: Long): F[Chunk[Byte]] = ???

    /** Reads a byte at `offset` from the start of the source.  */
    def apply(offset: Long): F[Byte] =
      read(1, offset).flatMap: chunk =>
        chunk.head match
          case Some(head) => head.pure
          case None       => F.raiseError(IOException(f"offset $offset%,d is beyond EOF"))
