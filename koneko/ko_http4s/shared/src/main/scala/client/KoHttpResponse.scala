package jp.ukiba.koneko
package ko_http4s
package client

import org.http4s.{Response, EntityDecoder, Status}
import org.http4s.client.UnexpectedStatus
import cats.effect.{Concurrent, Resource}
import cats.syntax.all.*

/**
  A combination of a undecoded HTTP response and the corresponding [[KoHttpRequest]].

  @tparam A the expected type of response body
*/
case class KoHttpResponse[F[_]: Concurrent, A]( // EntityDecoder.text requires Concurrent
  resource: Resource[F, Response[F]],
  req: KoHttpRequest[F, A],
):
  import KoHttpResponse.*
  type Self = KoHttpResponse[F, A]

  def expectSuccess: Self = expectStatus(_.responseClass == Status.Successful)

  def expectStatus(predicate: Status => Boolean): Self = copy(resource = resource.evalMap {
    case resp if predicate(resp.status) => resp.pure[F]
    case resp =>
        for {
          entity <- resp.as[String] // `UTF-8` unless `Content-Type` has a charset
          unexpectedStatus = UnexpectedStatus(resp.status, req.method, req.uri)
          _ <- Concurrent[F].raiseError(UnexpectedStatusAndEntity(unexpectedStatus, entity))
        } yield resp
  })

  def decode: F[Decoded[F, A]] = resource.use: resp =>
    req.decoder.decode(resp, req.decodeStrict).value.flatMap: either =>
      either match
        case Right(body) => Decoded(body, resp).pure[F]
        case Left(decodeFailure) => Concurrent[F].raiseError(decodeFailure)

  /** A combination of `expectSuccess` and `decode` */
  def decodeSuccess: F[Decoded[F, A]] = expectSuccess.decode

object KoHttpResponse:
  /** A decoded HTTP response. */
  case class Decoded[F[_], B](
    body: B,
    resp: Response[F], // holds headers
  )

  /* org.http4s.client.UnexpectedStatus doesn't have the message */
  // https://github.com/http4s/http4s/blob/v1.0.0-M40/client/shared/src/main/scala/org/http4s/client/Client.scala#L339
  case class UnexpectedStatusAndEntity(unexpectedStatus: UnexpectedStatus, entity: String)
      extends RuntimeException(unexpectedStatus) { // extends RuntimeException like UnexpectedStatus
    export unexpectedStatus.{status, requestMethod, requestUri}

    override def getMessage: String = if (entity.nonEmpty) entity else unexpectedStatus.getMessage
  }
