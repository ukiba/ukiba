package jp.ukiba.koneko
package ko_aws

import jp.ukiba.koneko.ko_munit.KoCatsEffectSuite

import org.http4s.ember.client.EmberClientBuilder
import org.typelevel.log4cats.{Logger, LoggerFactory}
import cats.effect.Resource
import cats.syntax.all.*

trait AwsSuite extends KoCatsEffectSuite:
  given log: Logger[F]

  override def munitFixtures = List(clientFixture, profileFixture)

  val clientFixture = ResourceSuiteLocalFixture("client",
    EmberClientBuilder.default[F].build
  )
  def client = clientFixture()

  val profileFixture = ResourceSuiteLocalFixture("profile",
    Resource.eval(AwsSdk.defaultProfile[F])
  )
  def profile = profileFixture()
