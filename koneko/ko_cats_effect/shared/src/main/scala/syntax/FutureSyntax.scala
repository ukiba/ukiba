package jp.ukiba.koneko
package ko_cats
package effect
package syntax

import cats.effect.Async
import cats.syntax.all.*

import scala.concurrent.Future

trait FutureSyntax:
  extension [A](future: Future[A])
    def toAsync[F[_]: Async]: F[A] = Async[F].fromFuture(future.pure[F])
