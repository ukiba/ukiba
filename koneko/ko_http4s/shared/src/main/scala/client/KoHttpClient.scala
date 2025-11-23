package jp.ukiba.koneko
package ko_http4s
package client

import org.http4s.client.Client
import org.http4s.{Request, EntityDecoder}
import org.typelevel.log4cats.Logger
import cats.effect.{Temporal, Resource}
import cats.syntax.all.*
import scodec.bits.ByteVector

import scala.concurrent.duration.FiniteDuration

/**
  A combination of an HTTP client and a base [[KoHttpRequest]].

  To initialize
  {{{
    EmberClientBuilder.default[F].build.use: client =>
      val api = KoHttpClient(client).withUri(uri"http://api.zippopotam.us/us/")
  }}}

  To use
  {{{
      api.GET(path"12561").acceptJson[PostCode]).decodeSuccess.toBody
  }}}

  @tparam A An expected type of response body (often overridden)
*/
case class KoHttpClient[F[_]: Temporal, A](
  client: Client[F],
  req: KoHttpRequest[F, A],
  logConf: KoHttpLog.Conf,
) extends KoHttpRequest.UnderlyingOp[F, KoHttpClient[F, A]] with KoHttpLog[F]:
  protected def underlying: Request[F] = req.underlying
  protected def withUnderlying(underlying: Request[F]): KoHttpClient[F, A] =
      copy(req = req.withUnderlying(underlying))

  def withDecoder[B](decoder: EntityDecoder[F, B]): KoHttpClient[F, B] = copy(req = req.withDecoder(decoder))
  def acceptJson[B](using decoder: EntityDecoder[F, B]): KoHttpClient[F, B] = copy(req = req.acceptJson)
  def acceptString: KoHttpClient[F, String] = copy(req = req.acceptString)
  def acceptByteVector: KoHttpClient[F, ByteVector] = copy(req = req.acceptByteVector)

  /** Performs an HTTP method */
  def run(using Logger[F]): KoHttpResponse[F, A] =
    val resp = for {
      _         <- Resource.eval(logReq(logConf)(req.underlying))
      beginTime <- Resource.eval(Temporal[F].realTime)
      resp      <- client.run(req.underlying)
      endTime   <- Resource.eval(Temporal[F].realTime)
      _         <- Resource.eval(logResp(logConf)(resp, endTime - beginTime, req.underlying))
    } yield resp

    KoHttpResponse(resp, req)

object KoHttpClient:
  def apply[F[_]: Temporal](
    client: Client[F],
    logConf: KoHttpLog.Conf = KoHttpLog.Conf.Min,
  ): KoHttpClient[F, ByteVector] = KoHttpClient(client, KoHttpRequest[F], logConf)

  /** To be given to RetryPolicy */
  def retryLogic[F[_]](retryIntervals: Seq[FiniteDuration])(numFailed: Int)
      (using log: Logger[F]): Option[FiniteDuration] = {
    Option.when(numFailed <= retryIntervals.size) {
      val interval = retryIntervals(numFailed - 1)
      log.debug(s"retrying in $interval")
      interval
    }
  }
