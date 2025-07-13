package jp.ukiba.koneko
package ko_aws
package s3

import jp.ukiba.koneko.ko_http4s.client.KoHttpClient

import org.typelevel.log4cats.{Logger, LoggerFactory}
import cats.syntax.all.*

class ListBucketsTests extends AwsSuite:
  given log: Logger[F] = LoggerFactory[F].getLogger

  nest("params"):
    test("none"):
      for
        http = KoHttpClient(client).withUri(endpointOf(profile.region))
        req <- ListBuckets.Request().pure[F]
        resp <- ListBuckets(profile)(http)(req)
      yield
        println(s"resp = $resp")
        //println(s"  buckets = ${resp.Buckets.toList.mkString("\n            ")}")

    test("bucket-region"):
      for
        http = KoHttpClient(client).withUri(endpointOf(profile.region))
        req <- ListBuckets.Request(`bucket-region` = Some(AwsRegion.Tokyo)).pure[F]
        resp <- ListBuckets(profile)(http)(req)
      yield
        println(s"resp = $resp")
        //println(s"  buckets = ${resp.Buckets.toList.mkString("\n            ")}")

    test("max-buckets"):
      for
        http = KoHttpClient(client).withUri(endpointOf(profile.region))
        req <- ListBuckets.Request(`max-buckets` = Some(1)).pure[F]
        resp <- ListBuckets(profile)(http)(req)
      yield
        println(s"resp = $resp")
        //println(s"  buckets = ${resp.Buckets.toList.mkString("\n            ")}")
