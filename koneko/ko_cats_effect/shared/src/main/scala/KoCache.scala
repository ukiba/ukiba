// TODO migrate to Scala 3 syntax when evip-cloud does

package jp.ukiba.koneko
package ko_cats
package effect

import cats.effect.kernel.{Temporal, Deferred, Fiber}
import cats.syntax.all._

import scala.concurrent.duration.FiniteDuration

object KoCache {
  /** The value of AtomicCell */
  sealed trait State[+A] { // TODO Scala 3: could this be sum type or enum?
    def started: FiniteDuration
    def attr: A
  }

  /** The initial state */
  case object Empty extends State[Nothing] {
    def started = throw UnsupportedOperationException() // never called
    def attr    = throw UnsupportedOperationException() // never called
  }

  case class Executing[F[_]: Temporal, V, A](
    result: Deferred[F, Result[V, A]],
    fiber: Fiber[F, Throwable, Unit],
    started: FiniteDuration,
    attr: A,
  ) extends State[A]

  case class Result[V, A](
    value: Either[Throwable, V],
    completed: FiniteDuration,

    // the followings have the same values as the corresponding [[Executing]]
    started: FiniteDuration,
    attr: A,
  ) extends State[A]

  def apply[F[_]: Temporal]: ApplyPartiallyApplied[F] = new ApplyPartiallyApplied()

  class ApplyPartiallyApplied[F[_]: Temporal]{
    def of[V, A](generator: F[V], initialAttr: A): F[KoCache[F, V, A]] = for {
      cell <- KoAtomicCell[F].of[State[A]](Empty) // AtomicCell uses Mutex, plus Ref when not Async
      cache = new KoCache[F, V, A](generator, cell)

      // generate the initial value
      // this should not block since this is the fisrt usage of the mutex lock
      _ <- cache.evalUpdateConditionallyAndGet((_, _) => Some(initialAttr))

      // the caller should not observe the Empty State
      // since the `generator` is always executed inside the mutex lock and
      // the cell value has become Executing before the mutex lock is released
    } yield cache
  }
}
import KoCache._

/**
 * A cache of single value with
 * 1. [[cats.effect.Deferred]] computation, and
 * 2. [[cats.effect.std.Mutex]] lock to prevent concurrent computations.
 *
 * @tparam V value
 * @tparam A attribute, might be used for example to store the previous successful [[Result]]
 *           (be careful to avoid leaking the previous previous result in that case)
 */
class KoCache[F[_]: Temporal, V, A](
  generator: F[V],
  cell: KoAtomicCell[F, State[A]],
) {
  private def F = implicitly[Temporal[F]] // TODO use `inline def` and `summon` in Scala 3

  type Executing = KoCache.Executing[F, V, A]
  type Result    = KoCache.Result[V, A]

  /**
   * Update conditionally then get the value.
   * @param f a function that returns Some attribute when the value is to be regenerated
   */
  def evalUpdateConditionallyAndGet(f: (Either[Executing, Result], FiniteDuration) => Option[A]): F[Result] =
    cell.evalUpdateConditionallyAndGet { origState =>
      for {
        started <- F.realTime
      } yield {
        def exec(attr: A): F[Executing] =
          for {
            result <- Deferred[F, Result]
            fiber <- F.start {
              for {
                value <- generator.attempt
                completed <- F.realTime

                resultObj = new Result(value, completed, started, attr)
                _ <- result.complete(resultObj).flatMap { ret =>
                  if (ret) ().pure[F] else F.raiseError(new IllegalStateException(
                    s"already completed: started = $started, completed = $completed")).void
                }

                // replace Executing with Result to release Deferred and Fiber
                _ <- cell.getAndSet(resultObj).flatMap { prev =>
                  prev match {
                    case executing: Executing @unchecked if (executing.result eq result) => ().pure[F]
                    case _ => F.raiseError(new IllegalStateException(
                        s"unexpected State: $prev: started = $started, completed = $completed"))
                  }
                }

              } yield ()
            }
          } yield Executing(result, fiber, started, attr)

        origState match {
          case executing: Executing @unchecked =>
            f(Left(executing), started).map { attr =>
              executing.fiber.cancel *> // cancel the previous Execution
              exec(attr).widen[State[A]]
            }

          case result: Result @unchecked =>
            f(Right(result), started).map { attr =>
              exec(attr).widen[State[A]]
            }

          case Empty =>
            // the parameters of `f` will be ignored
            val initialAttr = f(null, null).getOrElse(throw new IllegalStateException("No initialAttr"))
            exec(initialAttr).widen[State[A]].some
        }
      }

    }.flatMap { state =>
      state match {
        case executing: Executing @unchecked => executing.result.get
        case result: Result       @unchecked => result.pure[F]
        case Empty => throw new IllegalStateException(s"unexpected State: $state")
      }
    }
}
