package jp.ukiba.koneko
package ko_munit

import org.typelevel.log4cats.testing.TestingLoggerFactory, TestingLoggerFactory.LogMessage
import org.typelevel.log4cats.extras.LogLevel
import munit.CatsEffectSuite
import cats.effect.IO
import cats.syntax.all.*

import scala.annotation.tailrec

trait KoCatsEffectSuite extends CatsEffectSuite with KoTestSuite:
  type F[A] = IO[A] // let `Async[F]` be resolved
  val  F    = IO

  given TestingLoggerFactory[F] = TestingLoggerFactory.atomic[F]()

  /** @param pred predicate to filter the log entries */
  def showLogged(pred: LogMessage => Boolean)(using testLogFac: TestingLoggerFactory[F]): F[Seq[String]] =
    for
      logged <- testLogFac.logged
    yield
      logged.withFilter(pred).map(_.show)

  /** @param pred predicate to filter the log entries */
  def printLogged(pred: LogMessage => Boolean)(using testLogFac: TestingLoggerFactory[F]): F[Seq[String]] =
    for
      logged <- showLogged(pred)
    yield
      println(s"logged (${logged.size})")
      for (logged <- logged)
        println(s"  $logged")
      logged

  def printLoggedDebug(using testLogFac: TestingLoggerFactory[F]): F[Seq[String]] =
    printLogged(_.level >= LogLevel.Debug)

  // currently there is no way to prune fs2 stack frames
  override def munitValueTransforms = reduceNoiseTransform +: super.munitValueTransforms

  private val reduceNoiseTransform: ValueTransform = new ValueTransform("reduceNoise", {
    case io: IO[?] =>
      val improvedIo: IO[?] = io.adaptError:
        case ex => reduceNoise(ex)
      improvedIo.unsafeToFuture() // bypasses timeout in https://github.com/typelevel/munit-cats-effect/blob/main/core/src/main/scala/munit/CatsEffectSuite.scala
  })

  private def reduceNoise(ex: Throwable): Throwable =
    @tailrec
    def loop(ex: Throwable | Null): Unit = ex match
      case null          => ()
      case ex: Throwable =>
        ex.setStackTrace(ex.getStackTrace.filterNot(isNoise))
        loop(ex.getCause)
    loop(ex)
    ex

  private inline def isNoise(fr: StackTraceElement): Boolean =
    val className = fr.getClassName
    val methodName = fr.getMethodName
    className.contains(" @ jp.ukiba.koneko.ko_munit.") ||
        className.endsWith(" @ org.http4s.client.middleware.Retry$") ||
        className.contains(" @ org.http4s.ember.") ||
        (className == "map @ org.http4s.HttpDate$" && methodName == "current") ||
        className.contains(" @ org.typelevel.keypool.") ||
        className.startsWith("fs2.") ||
        className.contains(" @ fs2.") ||
        className.contains(" @ org.typelevel.log4cats.") ||
        className.contains(" @ munit.catseffect.")
