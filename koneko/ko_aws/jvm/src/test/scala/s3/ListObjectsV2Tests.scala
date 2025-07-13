package jp.ukiba.koneko
package ko_aws
package s3

import jp.ukiba.koneko.ko_http4s.client.KoHttpClient

import org.typelevel.log4cats.{Logger, LoggerFactory}
import cats.syntax.all.*

class ListObjectsV2Tests extends AwsSuite:
  given log: Logger[F] = LoggerFactory[F].getLogger

  val bucketName = "ko-aws-test-2025-07" // has to exist

  nest("params"):
    test("none"):
      for
        http = KoHttpClient(client).withUri(endpointOf(profile.region))
        req <- ListObjectsV2.Request(bucketName).pure[F]
        resp <- ListObjectsV2(profile)(http)(req)
      yield
        println(s"resp = $resp")
        println(s"  contents = ${resp.Contents.toList.mkString("\n             ")}")
