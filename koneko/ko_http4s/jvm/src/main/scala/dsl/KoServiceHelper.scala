package jp.ukiba.koneko
package ko_http4s
package dsl

import org.http4s.dsl.Http4sDsl
import org.http4s.headers.{`Content-Type`, Accept, Host, `X-Forwarded-Proto`}
import org.http4s._
import cats.effect.Sync
import cats.syntax.all._
import org.typelevel.ci.CIStringSyntax

trait KoServiceHelper[F[_]] extends Http4sDsl[F] {
  object MessageFailure {
    trait MessageFailureBase extends MessageFailure {
      def sanitized: String
      def detail: String
      def status: Status

      // https://github.com/http4s/http4s/blob/v1.0.0-M40/core/shared/src/main/scala/org/http4s/MessageFailure.scala#L29
      // MessageFailure extends RuntimeException but
      // Exception.{getMessage, getCause} are overridden to refer message/cause member

      override def message = s"$status: $sanitized: $detail" // for logging
      override def toHttpResponse[F[_]](httpVersion: HttpVersion) =
          Response(status, httpVersion).withEntity(sanitized) // excludes detail
    }

    // 400 Bad Request
    case class BadRequest(sanitized: String, detail: String = "",
        cause: Option[Throwable] = None) extends MessageFailureBase {
      def status = Status.BadRequest
    }

    object BadRequest {
      def apply(sanitized: String, cause: Throwable): BadRequest = BadRequest(sanitized, cause = Option(cause))

      def unexpectedContentType(contentType: `Content-Type`): BadRequest =
          BadRequest(s"Unexpected Content-Type header: ${contentType.mediaType}")

      def unexpectedContentType(contentType: Option[`Content-Type`]): BadRequest = contentType match {
        case Some(contentType) => unexpectedContentType(contentType)
        case None              => BadRequest(s"Missing Content-Type header")
      }

      def unexpectedAccept(accept: Accept): BadRequest = 
          BadRequest(s"Unexpected Accept header: ${accept.values.toList.mkString(", ")}")

      def unexpectedAccept(accept: Option[Accept]): BadRequest = accept match {
        case Some(accept) => unexpectedAccept(accept)
        case None         => BadRequest(s"Missing Accept header")
      }
    }

    // 401 not authenticated (unlike the name suggests)
    case class Unauthorized(sanitized: String, detail: String = "",
        cause: Option[Throwable] = None) extends MessageFailureBase {
      def status = Status.Unauthorized
    }

    // 403 not authorized
    case class Forbidden(sanitized: String, detail: String = "",
        cause: Option[Throwable] = None) extends MessageFailureBase {
      def status = Status.Forbidden
    }

    // 404 Not Found
    case class NotFound(sanitized: String, detail: String = "",
        cause: Option[Throwable] = None) extends MessageFailureBase {
      def status = Status.NotFound
    }

    // 502 Bad Gateway
    case class BadGateway(sanitized: String, detail: String = "",
        cause: Option[Throwable] = None) extends MessageFailureBase {
      def status = Status.BadGateway
    }
    object BadGateway {
      def apply(sanitized: String, cause: Throwable): BadGateway = BadGateway(sanitized, cause = Option(cause))
    }
  }

  object RequestUri {
    // http4s 1.0.0-M40: req.uri.{scheme, authority} are None
    def apply(req: Request[F]): Uri = {
      val scheme = req.uri.scheme.getOrElse {
        req.headers.get[`X-Forwarded-Proto`] match {
          case Some(proto) => proto.scheme
          case None => if (req.isSecure.exists(_ == true)) Uri.Scheme.https else Uri.Scheme.http
        }
      }

      val authority = req.uri.authority match {
        case Some(authority) if (authority.host.toString.nonEmpty) => authority
        case _ =>
          // http4s 1.0.0-M40: X-Forwarded-Host has not been implemented
          // https://github.com/http4s/http4s/pull/6979
          val (hostValue, port) = req.headers.get(ci"X-Forwarded-Host") match {
            case Some(nel) =>
              // MDN: X-Forwarded-Host has no port while Host does
              // https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/X-Forwarded-Host
              // https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Host
              (nel.head.value, None)

            case None =>
              req.headers.get[Host] match {
                case Some(hostHeader) => (hostHeader.host, hostHeader.port)
                case None => throw MessageFailure.BadRequest("No Host header")
              }
          }

          val host = Uri.Host.fromString(hostValue).fold(parseFailure =>
              throw MessageFailure.BadRequest(s"Host header: ${parseFailure.sanitized}"), identity)
          Uri.Authority(req.uri.authority.flatMap(_.userInfo), host, port)
      }

      req.uri.copy(scheme = Some(scheme), authority = Some(authority))
    }
  }
}
