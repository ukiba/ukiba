package jp.ukiba.koneko
package ko_http4s
package client

import jp.ukiba.koinu.ko_java.toHexString

import org.http4s.headers.`Content-Type`
import org.http4s.{Request, Response, Entity, Headers}
import org.typelevel.log4cats.Logger
import cats.effect.{Concurrent, Resource}
import cats.syntax.all.*
import scodec.bits.ByteVector

import scala.concurrent.duration.FiniteDuration
import scala.util.control.NonFatal
import scala.math.min
import scala.annotation.tailrec
import java.nio.charset.StandardCharsets

/** Logs HTTP requests and responses. */
trait KoHttpLog[F[_]: Concurrent]:
  import KoHttpLog.*

  def logReq(conf: Conf)(req: Request[F])(using log: Logger[F]): F[Unit] =
    for
      _ <- log.debug(s"${req.method} ${req.uri} ${
          logMessageOfEntity(req.entity, req.contentType, conf.reqBodyMaxLen)}")
      _ <- if (conf.reqHeaders) log.debug(s"headers = ${logMessageOfHeaders(req.headers)}") else ().pure[F]
    yield ()

  def logResp(conf: Conf)(resp: Response[F], duration: FiniteDuration, req: Request[F])
      (using log: Logger[F]): F[Unit] =
    for
      _ <- log.debug(f"${req.method} ${req.uri}: ${resp.status} in ${duration.toMillis}%,dms ${
          logMessageOfEntity(resp.entity, resp.contentType, conf.respBodyMaxLen)}")
      _ <- if (conf.respHeaders) log.debug(s"headers = ${logMessageOfHeaders(resp.headers)}") else ().pure[F]
    yield ()

object KoHttpLog:
  case class Conf(
    reqHeaders: Boolean = false,
    respHeaders: Boolean = false,

    reqBodyMaxLen : Option[Int] = Some(40),
    respBodyMaxLen: Option[Int] = Some(40),
  )
  object Conf:
    object Min extends Conf()
    object Full extends Conf(true, true, Some(Int.MaxValue), Some(Int.MaxValue))

  /** Log message of Entity */
  def logMessageOfEntity[F[_]](entity: Entity[F], contentType: Option[`Content-Type`],
      bodyMaxLen: Option[Int]): String = entity match
    case Entity.Streamed(stream, len) => len match {
      case Some(len) => s"(stream body: ${logMessageOfNumBytes(len)})"
      case None      => s"(stream body: unknown length)"
    } // TODO logBody
    // Ask ChatGPT o1: Write Scala program using fs2: Given Stream[F, Byte], write all the bytes to a file, while printing the first 100 bytes to the console
/*
    def tapFirst100[F[_]: Sync]: Pipe[F, Byte, Byte] = {
      def go(s: Stream[F, Byte], counter: Int): Pull[F, Byte, Unit] = {
        s.pull.uncons.flatMap {
          case Some((chunk, tail)) =>
            val (toPrint, toPass) =
              if (counter >= 100) {
                (Chunk.empty[Byte], chunk)
              } else if (counter + chunk.size <= 100) {
                (chunk, chunk)
              } else {
                val (printChunk, passChunk) = chunk.splitAt(100 - counter)
                (printChunk, chunk)
              }
            val printPull = Pull.eval(toPrint.traverse_(b => Sync[F].delay(print(b.toChar))))
            val outputPull = Pull.output(toPass)
            printPull >> outputPull >> go(tail, counter + chunk.size)
          case None => Pull.done
        }
      }
      in => go(in, 0).stream
    }
*/

    case Entity.Strict(bytes) => s"(immediate body: ${logMessageOfNumBytes(bytes.size)})${
        bodyMaxLen.map(logMessageOfBody(bytes, contentType, _)).map(" " + _).mkString}"

    // suppress GET body unless the body is being logged
    case Entity.Empty => bodyMaxLen.map(_ => "(empty body)").mkString

  /** Log message of Headers */
  def logMessageOfHeaders(headers: Headers): String = headers.headers.mkString(", ")

  /** A number of bytes */
  def logMessageOfNumBytes(len: Long): String = len match {
    case 1 => "1 byte"
    case ken => f"$len%,d bytes"
  }

  /** A String truncated by a max length */
  def logMessageOfBody(bytes: ByteVector, contentType: Option[`Content-Type`], maxLen: Int): String =
    decodeUtf8(bytes, maxLen) match
      case Some((text, numUtf8Bytes)) =>
        s"$text${Option.when(numUtf8Bytes < bytes.size)("...").mkString}"

      case None =>
        s"${bytes.take(maxLen).toArray.toHexString}${Option.when(maxLen < bytes.size)("...").mkString}"

  /** @return the decoded string, and the number of bytes it corresponds */
  def decodeUtf8(bytes: ByteVector, maxLen: Int): Option[(String, Int)] =
    numUtf8Bytes(bytes, maxLen).flatMap: numBytes =>
      val decoder = StandardCharsets.UTF_8.newDecoder
      try
        val charBuf = decoder.decode(bytes.take(numBytes).toByteBuffer)
        Some((charBuf.toString, numBytes))
      catch
        case NonFatal(ex) => None

  /** @return Some when entirely UTF-8, or only the last continuaton bytes are truncated */
  def numUtf8Bytes(bytes: ByteVector, maxLen: Int): Option[Int] =
    val limit = min(bytes.size, maxLen)
    @tailrec def loop(i: Int): Int =
      if i < limit then
        val leadingByte = bytes(i)
        val numContinuationBytes =
          if      (leadingByte & 0x80) == 0x00 then 0 // 0xxxxxxx
          else if (leadingByte & 0xe0) == 0xc0 then 1 // 110xxxxx
          else if (leadingByte & 0xf0) == 0xe0 then 2 // 1110xxxx
          else if (leadingByte & 0xf8) == 0xf0 then 3 // 11110xxx
          else throw InvalidSequenceException(f"illegal leading byte $leadingByte%02X")

        if i + numContinuationBytes < limit then
          (0 until numContinuationBytes).foreach: continuationByte =>
            if ((continuationByte & 0xc0) == 0x80) // 10xxxxxx
              ()
            else
              throw InvalidSequenceException(f"illegal continuation byte $continuationByte%02X")
          loop(i + 1 + numContinuationBytes)

        else
          i // continuaton bytes are truncated

      else
        i

    try
      Some(loop(0))
    catch
      case ex: InvalidSequenceException => None

  // exception should not happen often, and using it simplifies the implementation
  class InvalidSequenceException(message: String) extends Exception(message)

/*
  def errorLogMessageOf[F[_]](duration: FiniteDuration, req: Request[F]): String =
    f"failed in ${duration.toMillis}%,dms: ${req.method} ${req.uri}"

  /** @return A charset if the content is to be logged as text */
  def logCharsetOf(contentType: Option[`Content-Type`]): Option[Charset] = contentType.flatMap { contentType =>
    contentType.charset.orElse {
      import MediaType.application
      contentType.mediaType match {
        case mediaType if (mediaType.isText ||  // heuristic
            mediaType.satisfies(application.json))
            => Some(Charset.`UTF-8`)
        case _ => None
      }
    }
  }
*/
