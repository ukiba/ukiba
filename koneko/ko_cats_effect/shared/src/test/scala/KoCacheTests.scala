package jp.ukiba.koneko
package ko_cats
package effect

import jp.ukiba.koneko.ko_munit.KoCatsEffectSuite

//import org.typelevel.log4cats.{Logger, LoggerFactory}
import cats.effect.std.AtomicCell
import cats.effect.{Async, Concurrent}
import cats.effect.syntax.all.*
import cats.syntax.all.*

import scala.concurrent.duration.DurationInt

class KoCacheTests extends KoCatsEffectSuite:
  //given Logger[F] = LoggerFactory[F].getLogger

  class IntendedException extends Exception

  test("noUpdate and update alternatively, then fail"):
    def noUpdate(cache: KoCache[F, Int, Unit], startValue: Int): F[Unit] =
      cache.evalUpdateConditionallyAndGet { case (state, time) =>
        state match
          case Right(cache.Result(Right(startValue), _, _, _)) =>
          case Right(cache.Result(_                , _, _, _)) => fail("Unexpected state", clues(state))
          case Left(cache.Executing(result         , _, _, _)) =>
        None
      }.map(_.value).assertEquals(Right(startValue))

    def update(cache: KoCache[F, Int, Unit], startValue: Int): F[Unit] =
      cache.evalUpdateConditionallyAndGet { case (state, time) =>
        state match
          case Right(cache.Result(Right(startValue), _, _, _)) =>
          case Right(cache.Result(_                , _, _, _)) => fail("Unexpected state", clues(state))
          case Left(cache.Executing(result         , _, _, _)) =>
        Some(())
      }.map(_.value).assertEquals(Right(startValue + 1))

    def doFail(cache: KoCache[F, Int, Unit]): F[Unit] =
      cache.evalUpdateConditionallyAndGet { case (state, time) =>
        throw IntendedException()
      }.intercept[IntendedException].void

    val io = for
      cell <- AtomicCell[F].of(0)
      cache <- KoCache[F].of[Int, Unit](cell.updateAndGet(_ + 1), ())

      _ <- noUpdate(cache, 1)
      _ <- update(cache, 1)
      _ <- noUpdate(cache, 2)
      _ <- update(cache, 2)

      _ <- doFail(cache)
      _ <- noUpdate(cache, 3)
      _ <- update(cache, 3)

      _ <- doFail(cache)
      _ <- update(cache, 4)
      _ <- noUpdate(cache, 5)
    yield ()

    List.fill(10000)(io).parSequence

/*
  test("wait between updates"):
    for
      cell <- AtomicCell[F].of(0)
      cache <- KoCache[F].of[Int, Unit](Async[F].sleep(100.milliseconds) *> cell.updateAndGet(_ + 1), ())

      _ <- cache.evalUpdateConditionallyAndGet { case (state, time) =>
        state match
          case Right(cache.Result(Right(1), _, _, _)) =>
          case _ => fail("Unexpected state", clues(state))
        None
      }.map(_.value).assertEquals(Right(1))

      _ <- cache.evalUpdateConditionallyAndGet { case (state, time) =>
        state match
          case Right(cache.Result(Right(1), _, _, _)) =>
          case _ => fail("Unexpected state", clues(state))
        Some(())
      }.map(_.value).assertEquals(Right(2))

      _ <- cache.evalUpdateConditionallyAndGet { case (state, time) =>
        state match
          case Right(cache.Result(Right(2), _, _, _)) =>
          case _ => fail("Unexpected state", clues(state))
        None
      }.map(_.value).assertEquals(Right(2))

      _ <- cache.evalUpdateConditionallyAndGet { case (state, time) =>
        state match
          case Right(cache.Result(Right(2), _, _, _)) =>
          case _ => fail("Unexpected state", clues(state))
        Some(())
      }.map(_.value).assertEquals(Right(3))
    yield ()
*/

  //test("no update and update alternatively"):
