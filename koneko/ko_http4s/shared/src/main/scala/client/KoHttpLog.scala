package jp.ukiba.koneko
package ko_http4s
package client

import jp.ukiba.koneko.ko_fs2.PrefixAndSuffixTap
import jp.ukiba.koinu.ko_java.encoding.{CharUtil, XmlEncodingSniffer}
import jp.ukiba.koinu.ko_java.Hex

import org.http4s.headers.`Content-Type`
import org.http4s.{Request, Response, Message, Entity, Headers, Header, Method}
import fs2.Chunk
import org.typelevel.log4cats.Logger
import cats.effect.{Concurrent, Resource}
import cats.syntax.all.*
import scodec.bits.ByteVector

import scala.concurrent.duration.FiniteDuration
import scala.util.control.NonFatal
import scala.math.min
import scala.annotation.tailrec
import java.nio.charset.{Charset, StandardCharsets}, StandardCharsets.UTF_8
import java.nio.charset.{IllegalCharsetNameException, UnsupportedCharsetException}

/** Logs HTTP requests and responses. */
trait KoHttpLog[F[_]: Concurrent]:
  import KoHttpLog.*

  def logReq(conf: Conf)(req: Request[F])
      (using log: Logger[F]): F[Request[F]] =
    for
      reqWithLog <- logEntity(s"${req.method} ${req.uri}",
          req, conf.reqBodyPrefixMaxLen, conf.reqBodySuffixMaxLen,
          req.method == Method.GET) // suppress GET body
      _ <- if (conf.reqHeaders) log.debug(renderHeaders(req.headers)) else ().pure[F]
    yield reqWithLog

  def logResp(conf: Conf)(resp: Response[F], duration: FiniteDuration, req: Request[F])
      (using log: Logger[F]): F[Response[F]] =
    for
      respWithLog <- logEntity(f"${req.method} ${req.uri}: ${resp.status} in ${duration.toMillis}%,dms",
          resp, conf.respBodyPrefixMaxLen, conf.respBodySuffixMaxLen)
      _ <- if (conf.respHeaders) log.debug(renderHeaders(resp.headers)) else ().pure[F]
    yield respWithLog

object KoHttpLog:
  case class Conf(
    reqBodyPrefixMaxLen : Int = 0,
    reqBodySuffixMaxLen : Int = 0,
    reqHeaders: Boolean = false,

    respBodyPrefixMaxLen: Int = 0,
    respBodySuffixMaxLen: Int = 0,
    respHeaders: Boolean = false,
  )

  /** Log message of Entity */
  def logEntity[F[_]: Concurrent, M <: Message](label: String, msg: M[F],
      bodyPrefixMaxLen: Int, bodySuffixMaxLen: Int, suppressEmptyBody: Boolean = false)
      (using log: Logger[F]): F[msg.SelfF[F]] = // scala 3.8.0-RC3 cannot prove M[F] == msg.SelfF[F]
    def msgSelf = msg.asInstanceOf[msg.SelfF[F]] // safe for http4s Request/Response

    msg.entity match
      case Entity.Streamed(stream, len) =>
        for
          _ <- log.debug:
            len match
              case Some(len) => s"$label: stream body: ${renderNumBytes(len)}"
              case None      => s"$label: stream body: unknown length"
          (bodyWithLog, snap) <- PrefixAndSuffixTap[Byte](bodyPrefixMaxLen, bodySuffixMaxLen)
              .tapOnImmutableChunks(msg.body)
        yield
          msg.withBodyStream:
            bodyWithLog.onFinalize:
              for
                snap <- snap
                _ <- log.debug(s"$label: stream body: ${
                    renderBody(snap.prefix, snap.suffix, snap.count, msg.contentType)}")
              yield ()

      case Entity.Strict(bytes) =>
        val prefix = Chunk.byteVector(bytes.take(bodyPrefixMaxLen))
        val suffix = Chunk.byteVector(bytes.takeRight(min(bytes.size - prefix.size, bodySuffixMaxLen)))
        log.debug(s"$label: strict body: ${
            renderBody(prefix, suffix, bytes.size, msg.contentType)}")
            .as(msgSelf)

      case Entity.Empty =>
        log.debug(if !suppressEmptyBody then s"$label: empty body" else label)
            .as(msgSelf)

  def renderHeaders(headers: Headers): String =
    val size = headers.headers.size
    val firstLine = size match
      case 0 | 1 => s"$size header"
      case _     => f"$size%,d headers"

    (firstLine +: headers.headers.map(_.toString)).mkString("\n  ")

  def renderNumBytes(len: Long): String = len match
    case 0 | 1 => s"$len byte"
    case _     => f"$len%,d bytes"

  def renderBody(prefix: Chunk[Byte], suffix: Chunk[Byte], len: Long,
      contentType: Option[`Content-Type`]): String =
    val charset = contentType.flatMap(_.charset).map(_.nioCharset).getOrElse:
      XmlEncodingSniffer.extractEncoding(prefix.toArray) match
        case Some(xmlEncoding) =>
          try
            Charset.forName(xmlEncoding)
          catch
            // Scala.js 1.20.2: java.nio.charset.IllegalCharsetNameException is not available
            //case _: (IllegalCharsetNameException | UnsupportedCharsetException) => UTF_8
            case _: IllegalArgumentException => UTF_8  // common exception type
        case None => UTF_8

    val (str, numRemainingBytes) = CharUtil.decodePartial(prefix.toByteBuffer, charset)
    if numRemainingBytes == 0 then
      str

    else
      // TODO try CharUtil.decodePartialBackward
      val hexDump = if prefix.size + suffix.size < len then
        Seq(
          Hex.dump(prefix.toArray).mkString("\n"), // FIXME don't construct array
          s"... (omitted ${renderNumBytes(len - prefix.size - suffix.size)})",
          Hex.dump(suffix.toArray).mkString("\n"), // FIXME don't construct array
        ).mkString("\n")
      else
        Hex.dump((prefix ++ suffix).toArray).mkString("\n") // FIXME don't construct array

      val contentTypeRendered = contentType
        .map: header =>
          summon[Header[`Content-Type`, ?]].value(header) // render as in the wire
        .getOrElse("no contentType")
      s"${renderNumBytes(len)}: $contentTypeRendered\n$hexDump"


/*
    decodeUtf8(bytes, maxLen) match
      case Some((text, numUtf8Bytes)) =>
        s"$text${Option.when(numUtf8Bytes < bytes.size)("...").mkString}"

      case None =>
        s"${bytes.take(maxLen).toArray.toHexString}${Option.when(maxLen < bytes.size)("...").mkString}"

  /** @return the decoded string, and the number of bytes it corresponds */
  def decodeUtf8(bytes: ByteVector, maxLen: Int): Option[(String, Int)] =
    numUtf8Bytes(bytes, maxLen).flatMap: numBytes =>
      val decoder = UTF_8.newDecoder
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
