package jp.ukiba.koneko
package ko_cats
package effect

import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.extras.{LogMessage, LogLevel}
import cats.effect.{IO, IOApp}
import cats.effect.metrics.CpuStarvationWarningMetrics

import scala.concurrent.duration.Duration
import java.util.concurrent.atomic.AtomicInteger

/** Helpers for `IOApp` */
object KoIOAppHelper {
  /**
   * Logs [the starvation](https://typelevel.org/cats-effect/docs/core/starvation-and-tuning)
   * warnings to Logger[IO] rather than Console[IO].
   * The original log message is
   *
   *     [WARNING] Your app's responsiveness to a new asynchronous event (such as a new connection, an upstream response, or a timer) was in excess of 100 milliseconds. Your CPU is probably starving. Consider increasing the granularity of your delays or adding more cedes. This may also be a sign that you are unintentionally running blocking I/O operations (such as File or InetAddress) without the blocking combinator.
   *
   * The log message would be shortened from the second appearance, because
   * it is too long when there multiple warnings (for example when the machine sleeps / wakes up).
   */
  trait LogCpuStarvation { this: IOApp =>
    def cpuStarvationLogger: Logger[IO]

    def firstCpuStarvationLogLevel     : LogLevel = LogLevel.Warn  // Warning like the original implementation
    def successiveCpuStarvationLogLevel: LogLevel = LogLevel.Debug // Debug to reduce the noise

    val cpuStarvationCounter = new AtomicInteger(0) // AtomicInteger is also used in IOApp

    // Overrides [IOApp](https://github.com/typelevel/cats-effect/blob/series/3.x/core/jvm/src/main/scala/cats/effect/IOApp.scala) methods

    // The original version calls CpuStarvationCheck.logWarning
    override protected def onCpuStarvationWarn(metrics: CpuStarvationWarningMetrics): IO[Unit] =
      if (cpuStarvationCounter.incrementAndGet() == 1)
        LogMessage.log(firstCpuStarvationLogMessage(metrics), cpuStarvationLogger)
      else
        LogMessage.log(successiveCpuStarvationLogMessage(metrics), cpuStarvationLogger)

    // Reimplements [CpuStarvationCheck](https://github.com/typelevel/cats-effect/blob/series/3.x/core/shared/src/main/scala/cats/effect/CpuStarvationCheck.scala) method

    // The original `logWarning` method is hard wired to Console[IO]
    def firstCpuStarvationLogMessage(metrics: CpuStarvationWarningMetrics): LogMessage =
      LogMessage(firstCpuStarvationLogLevel, None,
          firstCpuStarvationMessage(metrics.starvationInterval * metrics.starvationThreshold))

    // The original `mkWarning` method is private
    // The original message is preserved except `${format(when)} [WARNING] ` at the beginning
    // for which an equivalent text would be prepended by `logger`
    // `when` parameter is omitted because it comes from `IO.realTime` through `metrics.occurrenceTime`
    // for which an equivalent text would be prepended by `logger`
    def firstCpuStarvationMessage(threshold: Duration): String =
      s"""|${successiveCpuStarvationMessage(threshold)}
          | Your CPU is probably starving. Consider increasing the
          | granularity of your delays or adding more cedes. This may also be a sign that you
          | are unintentionally running blocking I/O operations (such as File or InetAddress)
          | without the blocking combinator.""".stripMargin.replaceAll("\n", "")

    // Log the short message after the first one

    def successiveCpuStarvationLogMessage(metrics: CpuStarvationWarningMetrics): LogMessage =
      LogMessage(successiveCpuStarvationLogLevel, None,
          successiveCpuStarvationMessage(metrics.starvationInterval * metrics.starvationThreshold))

    def successiveCpuStarvationMessage(threshold: Duration): String =
      s"""|Your app's responsiveness to a new asynchronous
          | event (such as a new connection, an upstream response, or a timer) was in excess
          | of $threshold.""".stripMargin.replaceAll("\n", "")
  }
}
