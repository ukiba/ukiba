package jp.ukiba.koneko
package ko_munit

import org.typelevel.log4cats.testing.TestingLoggerFactory, TestingLoggerFactory.LogMessage
import org.typelevel.log4cats.extras.LogLevel
import munit.CatsEffectSuite
import cats.effect.IO
import cats.syntax.all.*

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
