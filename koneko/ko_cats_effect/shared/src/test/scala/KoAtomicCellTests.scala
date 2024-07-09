package jp.ukiba.koneko
package ko_cats
package effect

import jp.ukiba.koneko.ko_munit.KoCatsEffectSuite

//import org.typelevel.log4cats.{Logger, LoggerFactory}
import cats.effect.{Async, Concurrent}
import cats.effect.syntax.all.*
import cats.syntax.all.*

class KoAtomicCellTests extends KoCatsEffectSuite:
  //given Logger[F] = LoggerFactory[F].getLogger

  nest("Async"):
    tests { KoAtomicCell[F].async(0) }

  nest("Concurrent"):
    tests { KoAtomicCell[F].concurrent(0) }

  val inputNums = (0 until 10000 * 5).toList

  def tests(factory: => F[KoAtomicCell[F, Int]])(using NestedTestNames) =
    test("get"):
      for
        cell <- factory
        _ <- cell.get.assertEquals(0)
      yield ()

    test("set"):
      for
        cell <- factory
        _ <- cell.set(-1)
        _ <- cell.get.assertEquals(-1)
      yield ()

    // evalModify are used from evalUpdate, evalUpdateAndGet, evalGetAndUpdate

    test("evalUpdate"):
      for
        cell <- factory
        _ <- inputNums.traverse: i =>
          for
            _ <- cell.evalUpdate(x => (x + 1).pure[F])
            _ <- cell.get.assertEquals(i + 1)
          yield ()
      yield ()

    test("evalUpdateAndGet"):
      for
        cell <- factory
        _ <- inputNums.traverse: i =>
          cell.evalUpdateAndGet(x => (x + 1).pure[F])
              .assertEquals(i + 1)
      yield ()

    test("evalGetAndUpdate"):
      for
        cell <- factory
        _ <- inputNums.traverse: i =>
          cell.evalGetAndUpdate(x => (x + 1).pure[F])
              .assertEquals(i)
      yield ()

    // modify are used from update, updateAndGet, getAndUpdate

    test("update"):
      for
        cell <- factory
        _ <- inputNums.traverse: i =>
          for
            _ <- cell.update(_ + 1)
            _ <- cell.get.assertEquals(i + 1)
          yield ()
      yield ()

    test("updateAndGet"):
      for
        cell <- factory
        _ <- inputNums.traverse: i =>
          cell.updateAndGet(_ + 1).assertEquals(i + 1)
      yield ()

    test("getAndUpdate"):
      for
        cell <- factory
        _ <- inputNums.traverse: i =>
          cell.getAndUpdate(_ + 1).assertEquals(i)
      yield ()

    test("getAndSet"):
      for
        cell <- factory
        _ <- inputNums.traverse: i =>
          cell.getAndSet(i + 1).assertEquals(i)
      yield ()

    // KoAtomicCell specific

    test("evalUpdateConditionallyAndGet"):
      for
        cell <- factory
        _ <- inputNums.traverse: i =>
          for
            _ <- cell.evalUpdateConditionallyAndGet(x => Option.when(i % 3 == 0)((x + 1).pure[F]).pure[F])
            _ <- cell.get.assertEquals(1 + i / 3)
          yield ()
      yield ()
