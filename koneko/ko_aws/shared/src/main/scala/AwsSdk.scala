package jp.ukiba.koneko
package ko_aws

import software.amazon.awssdk.auth.credentials.{AwsCredentials, AwsSessionCredentials, DefaultCredentialsProvider}
import software.amazon.awssdk.regions.providers.DefaultAwsRegionProviderChain
import software.amazon.awssdk.core.exception.SdkClientException
import org.typelevel.log4cats.Logger
import cats.effect.Async
import cats.syntax.all.*

object AwsSdk:
  def defaultCredentials[F[_]: Async](using log: Logger[F]): F[Aws.Credentials] =
    val F = Async[F]
    F.blocking:
      DefaultCredentialsProvider.create.resolveCredentials match
        case creds: AwsSessionCredentials => Aws.Credentials.WithSession(
          creds.accessKeyId,
          creds.secretAccessKey,
          Option(creds.sessionToken),
        )
        /*
          When the SSO token has expired, SdkClientException with null cause is thrown.
          There seems to be no way to detect it in the multiple profiles environment since
          `Token is expired` in the message seems to be for a specific profile.
        */

        case creds: AwsCredentials => Aws.Credentials.WithoutSession(
          creds.accessKeyId,
          creds.secretAccessKey,
        )

  def defaultRegion[F[_]: Async](using log: Logger[F]): F[String] =
    val F = Async[F]
    F.blocking:
      DefaultAwsRegionProviderChain.builder.build.getRegion.id
