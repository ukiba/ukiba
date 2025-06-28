package jp.ukiba.koneko
package ko_aws
package s3

import jp.ukiba.koneko.ko_munit.KoCatsEffectSuite
import jp.ukiba.koneko.ko_http4s.client.KoHttpClient

import org.http4s.ember.client.EmberClientBuilder
import org.typelevel.log4cats.{Logger, LoggerFactory}
import cats.syntax.all.*

class ListBucketsTests extends KoCatsEffectSuite:
  override def munitFixtures = List(clientFixture)

  val clientFixture = ResourceSuiteLocalFixture("client",
    EmberClientBuilder.default[F].build
  )
  def client = clientFixture()

  given Logger[F] = LoggerFactory[F].getLogger

  test("no param"):
    for
      profile <- AwsSdk.defaultProfile[F]
      http = KoHttpClient(client).withUri(endpointOf(profile.region))
      req <- ListBuckets.Request().pure[F]
      resp <- ListBuckets(profile)(http)(req)
    yield
      println(s"resp = $resp")
