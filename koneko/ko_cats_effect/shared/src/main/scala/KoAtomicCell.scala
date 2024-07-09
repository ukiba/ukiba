// TODO migrate to Scala 3 syntax when evip-cloud does

package jp.ukiba.koneko
package ko_cats
package effect

import cats.effect.std.{Mutex, AtomicCell}
import cats.effect.kernel.{Concurrent, Async, Ref}
import cats.syntax.all._

/**
 * [[cats.effect.std.AtomicCell]] extended with
 * 1. `evalUpdateAndGetConditionally`
 *
 * Based on https://github.com/typelevel/cats-effect/blob/v3.5.4/std/shared/src/main/scala/cats/effect/std/AtomicCell.scala
 * 1. This requires `Concurrent` to implement the common members.
 */
abstract class KoAtomicCell[F[_]: Concurrent, A] extends AtomicCell[F, A] {
  private def F = implicitly[Concurrent[F]] // TODO use `inline def` and `summon` in Scala 3

  def evalModify[B]   (f: A => F[(A, B)]): F[B]
  def evalUpdate      (f: A => F[A]): F[Unit] = evalModify(a => f(a).map(aa => (aa, ())))
  def evalUpdateAndGet(f: A => F[A]): F[A]    = evalModify(a => f(a).map(aa => (aa, aa)))
  def evalGetAndUpdate(f: A => F[A]): F[A]    = evalModify(a => f(a).map(aa => (aa,  a)))

  def modify[B](f: A => (A, B)): F[B] = evalModify(a => F.pure(f(a)))

  /**
   * @param f produces Some value when this is to be updated to the value
   * @return the new value when updated, or the current value when not updated
   */
  def evalUpdateConditionallyAndGet(f: A => F[Option[F[A]]]): F[A]
}

object KoAtomicCell {
  def apply[F[_]: Concurrent]: ApplyPartiallyApplied[F] = new ApplyPartiallyApplied()

  class ApplyPartiallyApplied[F[_]: Concurrent]{
    def F = implicitly[Concurrent[F]] // TODO use `inline def` and `summon` in Scala 3

    def of[A](init: A): F[KoAtomicCell[F, A]] =
      F match {
        case f: Async[F] =>
          implicit val asyncF: Async[F] = f
          async(init)

        case _ =>
          concurrent(init)
      }

    // the followings are used by KoAtomicCellTests

    def async[F[_]: Async, A](init: A): F[KoAtomicCell[F, A]] =
      Mutex.apply[F].map(mutex => new AsyncImpl(init, mutex))

    def concurrent[F[_]: Concurrent, A](init: A): F[KoAtomicCell[F, A]] =
      (Ref.of[F, A](init), Mutex.apply[F]).mapN { (ref, mutex) => new ConcurrentImpl(ref, mutex) }

    //omitted: AtomicCell.ApplyPartiallyApplied.empty 
  }

  class ConcurrentImpl[F[_]: Concurrent, A](
    ref: Ref[F, A],
    mutex: Mutex[F]
  ) extends KoAtomicCell[F, A] {
    private def F = implicitly[Concurrent[F]] // TODO use `inline def` and `summon` in Scala 3

    def get: F[A] = ref.get
    def set(a: A): F[Unit] = mutex.lock.surround(ref.set(a))

    def evalModify[B](f: A => F[(A, B)]): F[B] = mutex.lock.surround {
      get.flatMap(f).flatMap { case (a, b) => ref.set(a).as(b) }
    }

    def evalUpdateConditionallyAndGet(f: A => F[Option[F[A]]]): F[A] = mutex.lock.surround {
      get.flatMap { a1 =>
        f(a1).flatMap { opt =>
          opt match {
            case Some(fa) => fa.flatMap(a2 => ref.set(a2).as(a2))
            case None     => a1.pure[F]
          }
        }
      }
    }
  }

  class AsyncImpl[F[_]: Async, A](
    init: A,
    mutex: Mutex[F]
  ) extends KoAtomicCell[F, A] {
    private def F = implicitly[Async[F]] // TODO use `inline def` and `summon` in Scala 3

    @volatile private var cell: A = init

    def get: F[A] = F.delay { cell }
    def set(a: A): F[Unit] = mutex.lock.surround { F.delay { cell = a } }

    def evalModify[B](f: A => F[(A, B)]): F[B] = mutex.lock.surround {
      get.flatMap(f).flatMap { case (a, b) => F.delay { cell = a; b } }
    }

    def evalUpdateConditionallyAndGet(f: A => F[Option[F[A]]]): F[A] = mutex.lock.surround {
      get.flatMap { a1 =>
        f(a1).flatMap { opt =>
          opt match {
            case Some(fa) => fa.flatMap(a2 => F.delay { cell = a2; a2 })
            case None     => a1.pure[F]
          }
        }
      }
    }
  }
}
