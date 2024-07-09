package jp.ukiba.koneko
package ko_cats
package effect
package syntax

import cats.effect.Async
import cats.syntax.all.*

import scala.scalajs.js

trait PromiseSyntaxPlatform:
  extension [A](promise: js.Promise[A])
    def toAsync[F[_]: Async]: F[A] = Async[F].fromPromise(promise.pure[F])
