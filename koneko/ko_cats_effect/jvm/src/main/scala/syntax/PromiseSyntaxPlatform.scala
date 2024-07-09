package jp.ukiba.koneko
package ko_cats
package effect
package syntax

import cats.effect.Async
import cats.effect.syntax.all.*
import cats.syntax.all.*

import scala.concurrent.Promise

trait PromiseSyntaxPlatform:
  extension [A](promise: Promise[A])
    def toAsync[F[_]: Async]: F[A] = Async[F].fromFuture(promise.future.pure[F])
