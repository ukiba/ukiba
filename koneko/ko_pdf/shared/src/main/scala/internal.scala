package jp.ukiba.koneko
package ko_pdf

import fs2.io.file.FileHandle
import fs2.Chunk
import cats.effect.Sync
import cats.syntax.all.*

import scala.annotation.tailrec
import java.nio.charset.{Charset, StandardCharsets}, StandardCharsets.UTF_8
import java.io.IOException

// TODO get rid of this
object internal:
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
  // almost the same as ko_java
  extension [O](chunk: Chunk[O])
    def countRight(pred: O => Boolean): Int =
      @tailrec def loop(count: Int, lastIndex: Int): Int =
        if (lastIndex >= 0 && pred(chunk(lastIndex)))
          loop(count + 1, lastIndex - 1)
        else
          count
      loop(0, chunk.size - 1)

    def dropRightWhile(pred: O => Boolean): Chunk[O] = chunk.dropRight(chunk.countRight(pred))

  // FIXME the followings do not belong here
  // the same as ko_java
  extension (bytes: Array[Byte])
    def parseString(cs: Charset): String = String(bytes, cs)
    def parseString: String = parseString(UTF_8)

  // FIXME the followings do not belong here
  // the same as ko_java
  abstract class ExceptionBase(message: String, cause: Throwable = null,
      enableSuppression: Boolean = true, writableStackTrace: Boolean = true)
      extends Exception(message, cause, enableSuppression, writableStackTrace):
    def this(cause: Throwable) = this(if cause != null then cause.getMessage else null, cause)
