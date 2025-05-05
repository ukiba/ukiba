package jp.ukiba.koneko
package ko_aws

import jp.ukiba.koinu.ko_java.{toHexString, utf8Bytes, `SHA2-256`, hmacSHA256}
import jp.ukiba.koinu.ko_java.time.atUtcZone

import software.amazon.awssdk.auth.credentials.{AwsCredentials, AwsSessionCredentials}
import org.http4s.{Request, Uri, Header, Headers}
import org.http4s.headers.Host
import cats.effect.Sync

import java.time.{Instant, ZoneOffset, ZonedDateTime}
import java.time.format.DateTimeFormatter

/**
 * Implements AWS Signature Version 4
 * https://docs.aws.amazon.com/AmazonS3/latest/API/sig-v4-authenticating-requests.html
 */
object AwsSigV4:
  val amzDateFmt   = DateTimeFormatter.ofPattern("yyyyMMdd'T'HHmmss'Z'") // ISO 8601
  val scopeDateFmt = DateTimeFormatter.ofPattern("yyyyMMdd")

  // https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_sigv-create-signed-request.html
  val defaultCanonicalHeaderNames: Seq[String] = Seq(
    "host",
    "content-type",
    "x-amz-*", // x-amz-* headers that you plan to include in your request must also be added
  )

  /*
    TODO could switch to AWS Signature Version 4A when region is not specified
  */
  def unsignedPayload[F[_]: Sync](
    req: Request[F],
    creds: AwsCredentials,
    region: String,
    service: String,
    canonicalHeaderNames: Seq[String] = defaultCanonicalHeaderNames, // supports glob at either end
    exeTime: Instant = Instant.now,
  ): F[Request[F]] = Sync[F].delay:
    // timestamps
    val amzDate   = exeTime.atUtcZone.format(amzDateFmt)
    val scopeDate = exeTime.atUtcZone.format(scopeDateFmt)

    // required headers
    val host = req.headers.get[Host].map(_.host)
        .orElse(req.uri.authority.map(_.host.renderString))
        .getOrElse("")
    val payload = "UNSIGNED-PAYLOAD"
    val requiredHeaders = Seq(
      "host"                 -> host, // host header (HTTP/1.1) or the :authority header (HTTP/2)
      "x-amz-content-sha256" -> payload,
      "x-amz-date"           -> amzDate,
    ) ++ (
      // include session token if present
      creds match
        case creds: AwsSessionCredentials => Seq("x-amz-security-token" -> creds.sessionToken)
        case _ => Nil
    )

    // canonical header
    val canonicalHeaders = (req.headers ++ Headers(requiredHeaders)).headers.flatMap: headerRaw =>
        val name = headerRaw.name.toString.toLowerCase // SignedHeaders must be in lowercase
        val matched = canonicalHeaderNames.exists: pattern =>
          if pattern.startsWith("*") then
            name.endsWith(pattern.drop(1))
          else if pattern.endsWith("*") then
            name.startsWith(pattern.dropRight(1))
          else
            name == pattern
        Option.when(matched):
          name -> headerRaw.sanitizedValue.trim // sanitize: new line chars to a space
      .sortBy(_._1)
    val signedHeadersNames = canonicalHeaders.map(_._1).mkString(";")

    // canonical request
    val canonicalUriPath = Uri.removeDotSegments(req.uri.path).segments.map(_.decoded())
    val canonicalQuery = req.uri.query.params.toSeq.sortBy(_._1)
    val canonicalReqHash = Seq(
      req.method.name,
      canonicalUriPath.map(Aws.uriEncode).mkString("/", "/", ""), // re-encode according to the AWS rule
      canonicalQuery.map { (key, value) => s"${Aws.uriEncode(key)}=${Aws.uriEncode(value)}" }.mkString("&"),
      canonicalHeaders.map { (key, value) => s"$key:$value\n" }.mkString, // newline at the end
      signedHeadersNames,
      payload
    ).mkString("\n").utf8Bytes.`SHA2-256`.toHexString

    // sign
    // https://docs.aws.amazon.com/AmazonS3/latest/API/sig-v4-header-based-auth.html
    val algo       = "AWS4-HMAC-SHA256" // AWS Signature Version 4
    val scopeComps = Seq(scopeDate, region, service, "aws4_request")
    val scope      = scopeComps.mkString("/")
    val toSign     = Seq(algo, amzDate, scope, canonicalReqHash).mkString("\n")

    inline def hmac(data: String, key: Array[Byte]): Array[Byte] = data.utf8Bytes.hmacSHA256(key)
    val sigKey = scopeComps.foldLeft(s"AWS4${creds.secretAccessKey}".utf8Bytes) { (key, comp) => hmac(comp, key) }
    val sig = hmac(toSign, sigKey).toHexString

    // Authorization header
    // https://docs.aws.amazon.com/AmazonS3/latest/API/sigv4-auth-using-authorization-header.html
    val credential = s"${creds.accessKeyId}/$scope"
    val authHeaderValue = s"$algo Credential=$credential, SignedHeaders=$signedHeadersNames, Signature=$sig"

    // override headers
    req.withHeaders(req.headers ++ Headers(canonicalHeaders) ++ Headers("Authorization" -> authHeaderValue))
