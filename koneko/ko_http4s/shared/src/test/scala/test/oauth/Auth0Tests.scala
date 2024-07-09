package jp.ukiba.koneko
package ko_http4s
package oauth

import client.KoHttpRequest
import client.syntax.all.*

import jp.ukiba.koneko.ko_munit.KoCatsEffectSuite
import org.http4s.{Headers, MediaType, Status, Charset}
import org.http4s.headers.`Content-Type`
import org.http4s.multipart.{Multipart, Part, Boundary}
import org.http4s.ember.client.EmberClientBuilder
import org.http4s.circe.CirceEntityEncoder.*
import org.http4s.circe.CirceEntityDecoder.*
import org.http4s.syntax.literals.* // uri, path
import org.typelevel.log4cats.{Logger, LoggerFactory}
import cats.effect.{IO, Async}
import cats.syntax.all.*
import io.circe, circe.syntax.*, circe.generic.auto.*

// testOnly -- *.Auth0Tests.*
class Auth0Tests extends KoCatsEffectSuite:
  override def munitFixtures = List(clientFixture)

  val clientFixture = ResourceSuiteLocalFixture("client",
    EmberClientBuilder.default[F].build
  )
  def client = clientFixture()

  given Logger[F] = LoggerFactory[F].getLogger

/*
  nest("password"):
    val api = KoHttpRequest[F].withUri(uri"https://dev-unkfwo8jxz2ymxrl.us.auth0.com/oauth/token")
    val client_id = "icaNUhKbkE6kXVEPWBSyy0qBaRLsMamI"
    val client_secret = "" // REPLACE THIS

    val usr1 = "usr1"
    val pass1 = "pass1PASS11"

    test("success"):
      for
        respBody <- client.run(api.POST
            .withEntity(AccessTokenRequest.password(usr1, pass1))
            .withBasicAuth(client_id, client_secret)
            .acceptJson[AccessTokenResponse])
            .decodeSuccess.toBody
            //.decodeSuccessOrError[Error].toBody
      yield
        ()
*/
