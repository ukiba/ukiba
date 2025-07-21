package jp.ukiba.koneko
package ko_aws
package s3

import jp.ukiba.koneko.ko_http4s.client.KoHttpClient

import org.http4s.headers.`Content-Length`
import fs2.Stream
import org.typelevel.log4cats.{Logger, LoggerFactory}
import cats.syntax.all.*

import scala.util.Random
import scala.math.min

class ObjectTests extends AwsSuite:
  given log: Logger[F] = LoggerFactory[F].getLogger

  val bucketName = "ko-aws-test-2025-07" // has to exist

  def contentStream(len: Long, seed: Long): Stream[F, Byte] =
    val rand = Random(seed)
    val chunkSize = 1024
    Stream.iterable(Iterable.range(0L, len, chunkSize.toLong)).flatMap: i =>
      Stream.emits(rand.nextBytes(min((len - i).toInt, chunkSize)))

  test("scenario"):
    val http = KoHttpClient(client).withUri(endpointOf(profile.region))

    def callListObjectsV2 =
      for
        req <- ListObjectsV2.Request(bucketName).pure[F]
        resp <- ListObjectsV2(profile)(http)(req)
      yield
        println(s"callListObjectsV2: resp = $resp")
        //println(s"  contents = ${resp.Contents.toList.mkString("\n             ")}")
        resp

    def callPutObject(key: String, content: Stream[F, Byte], len: Long) =
      for
        req <- PutObject.Request(content, key, bucketName, `Content-Length` = Some(`Content-Length`(len))).pure[F]
        resp <- PutObject(profile)(http)(req)
      yield
        println(s"callPutObject: resp = $resp")
        resp

    for
      list1 <- callListObjectsV2
      put1 <- callPutObject("random-1", contentStream(256 * 1024, 1), 256 * 1024)
    yield ()
