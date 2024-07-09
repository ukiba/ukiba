package jp.ukiba.koneko
package ko_http4s
package client
package syntax

import org.http4s.client.Client
import org.typelevel.log4cats.Logger
import cats.effect.{Temporal, Resource}

trait ClientSyntax:
  extension [F[_]: Temporal](client: Client[F])
    def run[A](req: KoHttpRequest[F, A], logConf: KoHttpLog.Conf = KoHttpLog.Conf.Min)
        (using Logger[F]): KoHttpResponse[F, A] = KoHttpClient(client, req, logConf).run
