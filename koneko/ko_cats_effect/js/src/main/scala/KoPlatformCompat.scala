package jp.ukiba.koneko
package ko_cats
package effect

import cats.effect.kernel.Clock

import java.time.Instant

object KoPlatformCompat:
  extension [F[_]](clock: Clock[F])
    // cats-effect 3.6.3: there is no realTimeInstant like in jvm
    // https://github.com/typelevel/cats-effect/blob/v3.6.3/kernel/jvm-native/src/main/scala/cats/effect/kernel/ClockPlatform.scala
    def realTimeInstant: F[Instant] =
      clock.applicative.map(clock.realTime)(duration => Instant.EPOCH.plusNanos(duration.toNanos))
