package jp.ukiba.koneko
package ko_aws

import software.amazon.awssdk.auth.credentials.{AwsCredentials, DefaultCredentialsProvider}
import org.typelevel.log4cats.Logger
import cats.effect.Async

object AwsSdk:
  type Credentials = AwsCredentials

  def defaultCredentials[F[_]: Async](using log: Logger[F]): F[Credentials] =
    Async[F].blocking:
      val creds = DefaultCredentialsProvider.create.resolveCredentials

      // e.g.
      // AwsCredentials(accessKeyId=..., providerName=StaticCredentialsProvider)
      // AwsSessionCredentials(accessKeyId=..., providerName=SsoCredentialsProvider, accountId=...)
      log.debug(s"creds = $creds")

      creds
