package jp.ukiba.koneko
package ko_aws
package s3

import jp.ukiba.koneko.ko_http4s.client.KoHttpClient

import org.http4s.headers.`Content-Length`
import fs2.Stream
import org.typelevel.log4cats.{Logger, LoggerFactory}
import cats.effect.syntax.all.*
import cats.syntax.all.*
import cats.data.Chain

class ObjectTests extends AwsSuite:
  given log: Logger[F] = LoggerFactory[F].getLogger

  val bucket = "ko-aws-test-2025-07" // has to exist
  def http = KoHttpClient(client)

  test("scenario"):
    for
      list1 <- ListObjectsV2(profile)(http)(ListObjectsV2.Request(bucket))

      // delete all the existing objects
      _ <- list1.Contents.parTraverseN(10): content =>
        DeleteObject(profile)(http)(DeleteObject.Request(bucket, content.Key))

      head1 <- HeadObject(profile)(http)(HeadObject.Request(bucket, "random-256k"))
      _ = assert(!head1.exists)

      put2 <- PutObject(profile)(http)(PutObject.Request(bucket, "random-256k", contentStream(256 * 1024, 1),
          `Content-Length` = Some(`Content-Length`(256 * 1024))))

      head2 <- HeadObject(profile)(http)(HeadObject.Request(bucket, "random-256k"))
      _ = assert(head2.exists)

      list2 <- ListObjectsV2(profile)(http)(ListObjectsV2.Request(bucket))
      _ = assertMatch(list2.Contents):
        case Chain(ListBucketResult.Contents("random-256k", _, _, _, _, _, _, size, _)) if size == 256 * 1024 =>

      get2 <- GetObject(profile)(http)(HeadObject.Request(bucket, "random-256k")).use: (respBody, resp) =>
        for
          _ = assertEquals(resp.ETag, head2.ETag)
          _ <- assertStreamsEquals(respBody, contentStream(256 * 1024, 1))
        yield ()

      logs <- showLogged(_ => true)
      //_ = println(logs.mkString("\n"))
    yield ()
