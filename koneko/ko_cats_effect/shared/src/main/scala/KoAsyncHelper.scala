package jp.ukiba.koneko
package ko_cats
package effect

import org.typelevel.log4cats.Logger
import cats.effect.Async
import cats.Applicative
import cats.syntax.all._

import scala.concurrent.duration.{FiniteDuration, Duration}

object KoAsyncHelper {
  /** Similar to `cats.effect.kernel.Clock.timed`, but based on `realTime` method rather than `monotonic`. */
  def timedReal[F[_]: Async, A](fa: F[A]): F[(FiniteDuration, A)] =
    // based on https://github.com/typelevel/cats-effect/blob/v3.5.2/kernel/shared/src/main/scala/cats/effect/kernel/Clock.scala#L55
    Applicative[F].map3(Async[F].realTime, fa, Async[F].realTime)((started, a, ended) => (ended - started, a))

  /**
   * Similar to `cats.effect.kernel.GenTemporal.sleep`, but based on `realTime` method.
   * @param duration negative values are treated as zero
   */
  def sleepReal[F[_]: Async: Logger](duration: FiniteDuration): F[Unit] = {
    if (duration <= Duration.Zero)
      Async[F].unit
    else {
      for {
        /*
          cats-effect 3.5.2 + Java 17.0.9 + macOS 13.4.1
          With `monotonic` method instead of `realTime`,
          this method can complete before elapsed time in `realTime` does not reach the duration.
        */
        elapsed <- timedReal(Async[F].sleep(duration)).map(_._1)

        /*
          cats-effect 3.1.0 + Java 11.0.10 + macOS 11.2.3
          IO.delayBy often waits 200ms ~ 2000ms less than the given time, for example when the duration is 1 hour.
        */
        _ <- if (elapsed < duration) {
          val remaining = duration - elapsed
          for {
            _ <- Logger[F].warn(s"sleep did not sleep enough: remaining = $remaining / $duration")
            _ <- sleepReal(remaining)
          } yield ()
        } else
          Async[F].unit
      } yield ()
    }
  }

  /**
   * Runs an effect at given period repeatedly.
   *
   * The period is considered to start at the epoch time.
   * For example, with a period of 1 minute, the effect would run at the beginning of every minute,
   * regardless of when this method is called.
   *
   * Unlike `fs2.Stream.{fixedRate, awakeEvery}`
   * 1. the effect will not run when the previous effect processing takes longer than the period.
   * 2. the time is measured with `realTime` rather than `monotonic`.
   *    `realTime` is `java.lang.System.currentTimeMillis` but
   *    `monotonic` is `java.lang.System.nanoTime` which can only be used to measure elapsed time and
   *    is not related to any other notion of system or wall-clock time.
   */
  def repeatPeriodically[F[_]: Async: Logger](period: FiniteDuration,
      onPeriod: FiniteDuration => F[Unit],
      onPeriodError: (Throwable, FiniteDuration) => F[Unit]): F[Unit] = for {
    _ <- Async[F].cede // try to increase the chance that this fiber is not interrupted for a while

    current <- Async[F].realTime // the system time
    next = period * ((current / period).toLong + 1) // next is always after current
    _ <- {
      for {
        _ <- sleepReal(next - current)
        _ <- onPeriod(next)
      } yield ()
    }.handleErrorWith(onPeriodError(_, next).handleErrorWith { ex =>
      Logger[F].error(ex)("onPeriodError raised an error")
    })

    _ <- repeatPeriodically(period, onPeriod, onPeriodError)
  } yield ()

  def repeatPeriodically[F[_]: Async: Logger](period: FiniteDuration,
      onPeriod: FiniteDuration => F[Unit]): F[Unit] =
    repeatPeriodically(period, onPeriod, defaultOnPeriodError[F])

  def defaultOnPeriodError[F[_]: Async: Logger](ex: Throwable, current: FiniteDuration): F[Unit] =
    Logger[F].error(ex)("onPeriod failed")

  /**
   * Runs an effect immediately and then at given period repeatedly.
   *
   * Named like `fs2.Stream.fixedRateStartImmediately`.
   */
  def repeatPeriodicallyStartImmediately[F[_]: Async: Logger](period: FiniteDuration,
      onPeriod: FiniteDuration => F[Unit],
      onPeriodError: (Throwable, FiniteDuration) => F[Unit]): F[Unit] = for {
    next <- Async[F].realTime
    _ <- onPeriod(next).handleErrorWith(onPeriodError(_, next).handleErrorWith { ex =>
      Logger[F].error(ex)("onPeriodError raised an error")
    })

    _ <- repeatPeriodically(period, onPeriod, onPeriodError)
  } yield ()

  def repeatPeriodicallyStartImmediately[F[_]: Async: Logger](period: FiniteDuration,
      onPeriod: FiniteDuration => F[Unit]): F[Unit] =
    repeatPeriodicallyStartImmediately(period, onPeriod, defaultOnPeriodError[F])
}
