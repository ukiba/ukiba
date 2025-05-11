package jp.ukiba.koneko
package ko_aws

import jp.ukiba.koneko.ko_http4s.client.KoHttpClient

import org.http4s.Uri
import org.typelevel.log4cats.Logger
import cats.effect.Async
import cats.syntax.all.*

/*
  2025-05
    https://github.com/gnieh/fs2-data is used since
    https://github.com/http4s/http4s-fs2-data has no releases for more than a year
    https://github.com/http4s/http4s-scala-xml seems to be replaced by http4s-fs2-data
*/
package object s3:
  def endpointOf(regionCode: String): Uri =
    Uri.unsafeFromString(s"https://s3.$regionCode.amazonaws.com/")

  object ListBuckets:
    def apply[F[_]: Async](creds: Aws.Credentials)(http: KoHttpClient[F, ?])(req: Request)
        (using Logger[F]): F[Response] =
      http.GET("")
          .withParamOpt("bucket-region"     , req.`bucket-region`)
          .withParamOpt("continuation-token", req.`continuation-token`)
          .withParamOpt("max-buckets"       , req.`max-buckets`)
          .withParamOpt("prefix"            , req.`prefix`)
          .evalMapRequest(AwsSigV4.unsignedPayload(_, creds, AwsRegion.Tokyo, "s3"))
          .flatMap { http => http.acceptString.run.decodeSuccess.map(_.body) }

    case class Request(
      `bucket-region`     : Option[String] = None,
      `continuation-token`: Option[String] = None,
      `max-buckets`       : Option[Int]    = None,
      `prefix`            : Option[String] = None,
    ):
      require(`continuation-token`.forall(str => 0 <= str.length && str.length <= 1024),
          s"continuation-token = ${`continuation-token`}")
      require(`max-buckets`.forall(num => 1 <= num && num <= 10000),
          s"max-buckets = ${`max-buckets`}")

    type Response = String
