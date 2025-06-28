package jp.ukiba.koneko
package ko_aws

import software.amazon.awssdk.auth.credentials.{AwsCredentials, AwsSessionCredentials, DefaultCredentialsProvider}
import software.amazon.awssdk.regions.providers.DefaultAwsRegionProviderChain
import software.amazon.awssdk.core.exception.SdkClientException
import org.typelevel.log4cats.Logger
import cats.effect.Async
import cats.syntax.all.*

object AwsSdk:
  /**
    @throws SdkClientException when not able to load, or
            when the SSO token is already expired (the cause is null).
            There seems to be no way to detect the expiration in the multiple profiles environment since
            `Token is expired` in the message seems to be for a specific profile.

    aws-sdk 2.31.x or later:
      SsoCredentialsProvider and SsoOidcTokenProvider automatically refreshes the token
  */
  def defaultCredentials[F[_]: Async](using log: Logger[F]): F[Aws.Credentials] =
    val F = Async[F]
    F.blocking:
      DefaultCredentialsProvider.create.resolveCredentials match
        case creds: AwsSessionCredentials => Aws.Credentials.WithSession(
          creds.accessKeyId,
          creds.secretAccessKey,
          Option(creds.sessionToken),
        )

        case creds: AwsCredentials => Aws.Credentials.WithoutSession(
          creds.accessKeyId,
          creds.secretAccessKey,
        )

  /** @throws SdkClientException when not able to load */
  def defaultRegion[F[_]: Async](using log: Logger[F]): F[String] =
    val F = Async[F]
    F.blocking:
      DefaultAwsRegionProviderChain.builder.build.getRegion.id

  /** @throws SdkClientException when not able to load */
  def defaultProfile[F[_]: Async](using log: Logger[F]): F[Aws.Profile] =
    for
      creds <- defaultCredentials
      region <- defaultRegion
    yield
      Aws.Profile(creds, region)
