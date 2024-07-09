package jp.ukiba.koneko
package ko_http4s
package client
package syntax

import KoHttpResponse.Decoded

import cats.effect.Concurrent
import cats.syntax.all.*

trait KoHttpResponseSyntax:
  extension [F[_]: Concurrent, A](decodedF: F[Decoded[F, A]])
    def toBody: F[A] = decodedF.map(_.body)
