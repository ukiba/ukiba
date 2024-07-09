package jp.ukiba.koneko
package ko_http4s

import org.http4s.{UrlForm, QueryParamEncoder}

package object oauth:
  // RFC 6749
  object AccessTokenRequest:
    // Resource Owner Password Credentials Grant
    // Including client_id and client_secret in the request-body is NOT RECOMMENDED and
    // SHOULD be limited to clients unable to directly utilize the HTTP Basic authentication
    def password(username: String, password: String, scope: Seq[String] = Nil): UrlForm = UrlForm(
      "grant_type" -> "password",
      "username" -> username,
      "password" -> password,
    ).updateFormField("scope", scope.mkString(" "))

    def refresh_token(refresh_token: String): UrlForm = UrlForm(
      "grant_type" -> "refresh_token",
      "refresh_token" -> refresh_token,
    )

  case class AccessTokenResponse(
    access_token : String,
    token_type   : String,
    expires_in   : Option[Int],
    refresh_token: Option[String],
    scope        : Option[String],
  )

  case class Error(
    error: String,
    error_description: Option[String],
    error_uri: Option[String],
    state: Option[String],
  )
