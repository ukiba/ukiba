package jp.ukiba.koneko
package ko_aws

import jp.ukiba.koinu.ko_java.truncate

import org.http4s.Uri
import cats.effect.Async
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

  /** RFC 3986 URI Unreserved Characters */
  def uriUnreservedChar(ch: Char): Boolean = ch match
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
    Uri.encode(decoded, toSkip = uriUnreservedChar)
