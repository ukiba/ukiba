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

  // TODO the followings should belong elsewhere
  import fs2.Stream
  import cats.effect.Sync

  import scala.util.Random
  import scala.math.min

  def assertStreamsEquals[F[_]: Sync, A](s1: Stream[F, A], s2: Stream[F, A]): F[Unit] =
    val F = Sync[F]

    // Lift bytes to Option so we can detect end‑of‑stream
    val s1opt = s1.map(Some(_)) ++ Stream.emit(None)
    val s2opt = s2.map(Some(_)) ++ Stream.emit(None)

    s1opt
      .zipAll(s2opt, None, None)
      .evalMap:
        case (Some(b1), Some(b2)) if b1 == b2 => F.unit
        case (Some(b1), Some(b2)) => F.raiseError(AssertionError(s"Byte mismatch: $b1 != $b2"))
        case (None, None) => F.unit // both ended together
        case (None, Some(_)) => F.raiseError(AssertionError("First stream ended before second"))
        case (Some(_), None) => F.raiseError(AssertionError("Second stream ended before first"))
      .compile
      .drain

  def contentStream(len: Long, seed: Long): Stream[F, Byte] =
    val rand = Random(seed)
    val chunkSize = 1024
    Stream.iterable(Iterable.range(0L, len, chunkSize.toLong)).flatMap: i =>
      Stream.emits(rand.nextBytes(min((len - i).toInt, chunkSize)))
