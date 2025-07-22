package jp.ukiba.koneko
package ko_aws

import jp.ukiba.koinu.ko_java.{toHexString, utf8, sha256, hmac}
import jp.ukiba.koinu.ko_java.time.atUtcZone

import org.http4s.{Request, Uri, Header, Headers}
import org.http4s.headers.Host
import cats.effect.Sync
import cats.syntax.all.*

import java.time.{Instant, ZoneOffset, ZonedDateTime}
import java.time.format.DateTimeFormatter

/**
 * AWS Signature Version 4
 * https://docs.aws.amazon.com/AmazonS3/latest/API/sig-v4-authenticating-requests.html
 */
object AwsSigV4:
  val amzDateFmt = DateTimeFormatter.ofPattern("yyyyMMdd'T'HHmmss'Z'") // ISO 8601
  val yyyyMMdd   = DateTimeFormatter.ofPattern("yyyyMMdd")

  // https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_sigv-create-signed-request.html
  val defaultCanonicalHeaderNames: Seq[String] = Seq(
    "host",
    "content-type",
    "x-amz-*", // x-amz-* headers that you plan to include in your request must also be added
  )

  // TODO could switch to AWS Signature Version 4A when region is not specified

  def `UNSIGNED-PAYLOAD`[F[_]: Sync](
    req: Request[F],
    creds: Aws.Credentials,
    region: String,
    service: String,
    canonicalHeaderNames: Seq[String] = defaultCanonicalHeaderNames, // supports glob at either end
  ): F[Request[F]] = for
    exeTime <- Sync[F].realTimeInstant

    `x-amz-content-sha256` <- "UNSIGNED-PAYLOAD".pure

    // timestamps
    amzDate = exeTime.atUtcZone.format(amzDateFmt)

    // required headers
    host =
      inline def render(host: String, port: Option[Int]): String =
        // MinIO drops the port in the signature calculation
        host//s"$host${port.map(":" + _).mkString}"
      req.headers.get[Host].map(host => render(host.host, host.port))
          .orElse(req.uri.authority.map(authority => render(authority.host.renderString, authority.port)))
          .getOrElse:
            throw IllegalArgumentException(s"No host is found in request")
    requiredHeaders = Seq(
      "host"                 -> host, // host header (HTTP/1.1) or the :authority header (HTTP/2)
      "x-amz-content-sha256" -> `x-amz-content-sha256`,
      "x-amz-date"           -> amzDate,
    ) ++ creds.sessionToken.toSeq.map: sessionToken =>
      "x-amz-security-token" -> sessionToken

    // canonical header
    canonicalHeaders = (req.headers ++ Headers(requiredHeaders)).headers.flatMap: headerRaw =>
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
    signedHeadersNames = canonicalHeaders.map(_._1).mkString(";")

    // canonical request
    canonicalUriPath = Uri.Path(
      Uri.removeDotSegments(req.uri.path).segments
          .map(_.decoded()).map(Aws.uriEncode).map(Uri.Path.Segment), // re-encode according to the AWS rule
      true, // absolute,
      req.uri.path.endsWithSlash
    )
    canonicalQuery = req.uri.query.params.toSeq.sortBy(_._1)
    canonicalReqHash = Seq(
      req.method.name,
      canonicalUriPath.toString,
      canonicalQuery.map { (key, value) => s"${Aws.uriEncode(key)}=${Aws.uriEncode(value)}" }.mkString("&"),
      canonicalHeaders.map { (key, value) => s"$key:$value\n" }.mkString, // newline at the end
      signedHeadersNames,
      `x-amz-content-sha256`, // hashedPayload
    ).mkString("\n").utf8.sha256.toHexString

    // sign
    // https://docs.aws.amazon.com/AmazonS3/latest/API/sig-v4-header-based-auth.html
    algo       = "AWS4-HMAC-SHA256" // AWS Signature Version 4
    scopeComps = Seq(exeTime.atUtcZone.format(yyyyMMdd), region, service, "aws4_request")
    scope      = scopeComps.mkString("/")
    toBeSigned = Seq(algo, amzDate, scope, canonicalReqHash).mkString("\n")

    signingKey = signingKeyOf(creds.secretAccessKey, scopeComps)
    signature = `HMAC-SHA256`(toBeSigned, signingKey).toHexString

    // Authorization header
    // https://docs.aws.amazon.com/AmazonS3/latest/API/sigv4-auth-using-authorization-header.html
    credential = s"${creds.accessKeyId}/$scope"
    authHeaderValue = s"$algo Credential=$credential, SignedHeaders=$signedHeadersNames, Signature=$signature"

  yield
    // override headers
    req.withHeaders(req.headers ++ Headers(canonicalHeaders) ++ Headers("Authorization" -> authHeaderValue))

  // For streaming,
  // x-amz-decoded-content-length must be given before streaming the chunks
  // https://docs.aws.amazon.com/AmazonS3/latest/API/sigv4-streaming.html

  def signingKeyOf(secretAccessKey: String, tail: Seq[String]): Array[Byte] =
    val head = s"AWS4$secretAccessKey".utf8
    tail.foldLeft(head): (intermediate, comp) =>
      `HMAC-SHA256`(comp, intermediate)

  inline def `HMAC-SHA256`(data: String, key: Array[Byte]): Array[Byte] = data.utf8.hmac.sha256(key)
