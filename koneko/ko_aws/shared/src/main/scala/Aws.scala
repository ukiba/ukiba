package jp.ukiba.koneko
package ko_aws

import jp.ukiba.koinu.ko_java.{truncate, replaceFirstLiterally}

import org.http4s.Uri
import cats.effect.Sync
import cats.syntax.all.*

object Aws:
  trait Credentials:
    def accessKeyId: String
    def secretAccessKey: String
    def sessionToken: Option[String]
    override def toString = s"Aws.Credentials(accessKeyId = ${accessKeyId .truncate(12)}${
        sessionToken.map(sessionToken => s", sessionToken = ${sessionToken.truncate(12)}").mkString})"

  object Credentials:
    case class WithoutSession(
      accessKeyId: String,
      secretAccessKey: String,
    ) extends Credentials:
      def sessionToken = None

    case class WithSession(
      accessKeyId: String,
      secretAccessKey: String,
      sessionToken: Option[String],
    ) extends Credentials

  /*
    https://docs.aws.amazon.com/cli/latest/userguide/cli-configure-files.html
    > Each profile can specify different credentials and can also specify different AWS Regions and output formats.

    https://docs.aws.amazon.com/sdk-for-java/latest/developer-guide/endpoint-config.html
    https://docs.aws.amazon.com/cli/v1/userguide/cli-configure-endpoints.html
    https://docs.aws.amazon.com/sdkref/latest/guide/feature-ss-endpoints.html

    SBT example to use a profile from the aws config:

        set ko_aws.jvm / Test / envVars ++= Map("AWS_PROFILE" -> "foo")

    SBT example to use local minio:

        set ko_aws.jvm / Test / envVars ++= Map("AWS_ENDPOINT_URL_S3" -> "http://localhost:9000", "AWS_ACCESS_KEY_ID" -> "minioadmin", "AWS_SECRET_ACCESS_KEY" -> "minioadmin", "AWS_REGION" -> "ap-northeast-1")
  */
  case class Profile[F[_]: Sync](
    credentials: F[Credentials],
    region: String,
    endpointUrl: Option[String]       = Profile.defaultEndpointUrl, // global
    endpointUrlPrefix: Option[String] = Profile.defaultEndpointUrlPrefix,
    endpointUrlSuffix: Option[String] = Profile.defaultEndpointUrlSuffix,
    useDualstackEndpoint: Boolean     = Profile.defaultUseDualstackEndpoint,
    useFipsEndpoint: Boolean          = Profile.defaultUseFipsEndpoint,
    pathStyleAccessEnabled: Option[Boolean] = None,
    //services: Map[String, Profile.Service] = Profile.defaultServices, // TODO cache endpointUrl of service
  ):
    def endpointUrlOf(service: String): String =
      import Profile.{defaultEndpointUrlOf, hasFipsEndpoint}
      val resolved = defaultEndpointUrlOf(service, region).orElse(endpointUrl).getOrElse:
        // https://docs.aws.amazon.com/general/latest/gr/rande.html
        // https://docs.aws.amazon.com/general/latest/gr/s3.html
        s"https://$service.$region.amazonaws.com/"

      (endpointUrlPrefix, endpointUrlSuffix) match
        case (None, None) if !useDualstackEndpoint && !useFipsEndpoint => resolved // the most common case
        case (prefix, suffix) =>
          var url = resolved
          if useDualstackEndpoint then
            url = url.replaceFirstLiterally(".", ".dualstack.")
          if useFipsEndpoint && hasFipsEndpoint(service, region) then
            url = url.replaceFirstLiterally(".", "-fips")
          s"${prefix.mkString}$url${suffix.mkString}" // TODO test

    def endpointUriOf(service: String): Uri = Uri.unsafeFromString(endpointUrlOf(service))

    def endpointUriOfS3(bucket: String): Uri =
      val base = endpointUriOf("s3")
      val pathStyleAccess = pathStyleAccessEnabled.getOrElse:
        base.authority.map(_.host) match
          case Some(Uri.RegName(host)) if host.toString.take(2) == "s3" => false
          case _                                                        => true
      if !pathStyleAccess then // default vartual-host style
        base.authority.map(_.host) match
          case Some(Uri.RegName(name)) =>
            base.copy(authority = base.authority.map(_.copy(host = Uri.RegName(s"$bucket.$name"))))
          case host =>
            throw IllegalStateException(s"pathStyleAccess: host is not a name: $host")
      else
        base.withPath(Uri.Path(
          Uri.Path.Segment(bucket) +: base.path.segments,
          absolute = true,
          endsWithSlash = true,
        ))

  object Profile:
    lazy val defaultEndpointUrl: Option[String] = firstStringOf(
      () => sys.env.get("AWS_ENDPOINT_URL"), // always None in Scala.js
      () => sys.props.get("aws.endpointUrl"), // probably None in Scala.js unless set
      // TODO aws config file
    )

    lazy val defaultEndpointUrlPrefix: Option[String] = firstStringOf(
      () => sys.env.get("AWS_ENDPOINT_URL_PREFIX"),
      () => sys.props.get("aws.endpointUrl.prefix"),
    )

    lazy val defaultEndpointUrlSuffix: Option[String] = firstStringOf(
      () => sys.env.get("AWS_ENDPOINT_URL_SUFFIX"),
      () => sys.props.get("aws.endpointUrl.suffix"),
    )

    lazy val defaultUseDualstackEndpoint: Boolean = firstBooleanOf(
      () => None// TODO aws config file: use_dualstack_endpoint
    )

    lazy val defaultUseFipsEndpoint: Boolean = firstBooleanOf(
      () => sys.env.get("AWS_USE_FIPS_ENDPOINT"),
      // TODO aws config file: use_fips_endpoint
    )

    def defaultEndpointUrlOf(service: String, region: String): Option[String] = firstStringOf(
      () => sys.env.get(s"AWS_ENDPOINT_URL_${service.toUpperCase}"),
      () => sys.props.get(s"aws.endpointUrl.${service.toLowerCase}"),
      // TODO aws config file
    )

    // https://aws.amazon.com/compliance/fips/#FIPS_Endpoints_by_Service
    def hasFipsEndpoint(service: String, region: String): Boolean = (service, region) match
      case ("aiops", "us-east-1" | "us-east-2" | "us-west-2") => true
      case ("apigateway", "us-east-1" | "us-east-2" | "us-west-1" | "us-west-2" |
          "us-gov-east-1" | "us-gov-west-1" |
          "ca-central-1" | "ca-west-1") => true
      // TODO...
      case _ => false

    private inline def firstStringOf(fns: () => Option[String]*): Option[String] =
      // collectFirst can be used with Function.unlift that converts fn to PartialFunction but it calls fn twice
      fns.iterator.flatMap(fn => fn()).nextOption // lazily find the first Some

    private inline def firstBooleanOf(fns: () => Option[String]*): Boolean =
      fns.iterator.flatMap:fn =>
        fn().map:
          case "true"  => true
          case "false" => false
          case str     => throw IllegalArgumentException(s"Unexpected Boolean: $str; must be true or false")
      .nextOption.getOrElse(false)

  /** RFC 3986 URI Unreserved Characters */
  def isUriUnreservedChar(ch: Char): Boolean = ch match
    // https://datatracker.ietf.org/doc/html/rfc3986#section-2.3
    // http4s-1.0.0-M44: implemented here since Uri.Unreserved is package private
    case ch if '0' <= ch && ch <= '9' => true
    case ch if 'A' <= ch && ch <= 'Z' => true
    case ch if 'a' <= ch && ch <= 'z' => true
    case '-' | '.' | '_' | '~'        => true
    case _ => false

  /**
     UriEncode in https://docs.aws.amazon.com/AmazonS3/latest/API/sigv4-query-string-auth.html
     @return encoded string
   */
  def uriEncode(decoded: String): String =
    Uri.encode(decoded, toSkip = isUriUnreservedChar)
