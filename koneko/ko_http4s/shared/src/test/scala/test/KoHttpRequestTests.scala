package jp.ukiba.koneko
package ko_http4s
package client

import syntax.all.*

import jp.ukiba.koneko.ko_munit.KoCatsEffectSuite
import org.http4s.{Headers, MediaType, Status, Charset}
import org.http4s.headers.`Content-Type`
import org.http4s.multipart.{Multipart, Part, Boundary}
import org.http4s.ember.client.EmberClientBuilder
import org.http4s.syntax.literals.* // uri, path
import org.typelevel.log4cats.{Logger, LoggerFactory}
import cats.effect.{IO, Async}
import cats.syntax.all.*
import io.circe, circe.syntax.*

import java.nio.charset.StandardCharsets

class KoHttpRequestTests extends KoCatsEffectSuite:
  override def munitFixtures = List(clientFixture)

  val clientFixture = ResourceSuiteLocalFixture("client",
    EmberClientBuilder.default[F].build
  )
  def client = clientFixture()

  given Logger[F] = LoggerFactory[F].getLogger

  // AWS
  nest("checkip.amazonaws.com"):
    def api = KoHttpClient(client).withUri(uri"https://checkip.amazonaws.com") // TODO move to KoHttpClientTests.scala

    test("200"):
      for
        myIp <- api.acceptString.GET.run.decodeSuccess.toBody
      yield
        assert(myIp.trim.matches("[0-9.]+"))


  // https://developers.google.com/speed/public-dns/docs/doh/json
  nest("dns.google"):
    def api = KoHttpClient(client).withUri(uri"https://dns.google") // TODO move to KoHttpClientTests.scala

    case class Result(
      Status: Int, // NOERROR - Standard DNS response code (32 bit integer).
      TC: Boolean, // Whether the response is truncated
      RD: Boolean, // Always true for Google Public DNS
      RA: Boolean, // Always true for Google Public DNS
      AD: Boolean, // Whether all response data was validated with DNSSEC
      CD: Boolean, // Whether the client asked to disable DNSSEC
      Question: Seq[Result.Question],
      Answer: Seq[Result.Answer],
    )
    object Result:
      case class Question(
        name: String, // FQDN with trailing dot
        `type`: Int,  // DNS RR type
      )
      case class Answer(
        name: String, // Always matches name in the Question section
        `type`: Int,  // DNS RR type
        TTL: Int,     // Record's time-to-live in seconds
        data: String, // Data for A - IP address as text
      )

    import org.http4s.circe.CirceEntityDecoder.*
    import io.circe.generic.auto.*

    test("200"):
      for
        result <- api.acceptJson[Result].GET("resolve?name=example.com&type=A").run.decodeSuccess.toBody
      yield
        () // TODO check the respBody

  // https://cloudflare.com/
  nest("cloudflare.com"):
    def api = KoHttpClient(client).withUri(uri"https://cloudflare.com") // TODO move to KoHttpClientTests.scala

    test("200"):
      for
        respBody <- api.acceptString.GET("cdn-cgi/trace").run.decodeSuccess.toBody
      yield
        () // TODO check the respBody

  // https://learn.microsoft.com/ja-jp/windows-server/networking/ncsi/ncsi-frequently-asked-questions
  nest("www.msftconnecttest.com"):
    def api = KoHttpClient(client).withUri(uri"http://www.msftconnecttest.com") // TODO move to KoHttpClientTests.scala

    test("200"):
      for
        respBody <- api.acceptString.GET("connecttest.txt").run.decodeSuccess.toBody
      yield
        assertEquals(respBody, "Microsoft Connect Test")

  // https://whatismyip.akamai.com/advanced?debug
  // https://ipv4.whatismyip.akamai.com
  // https://ifconfig.co/

  // https://api.zippopotam.us/
/*
  nest("zippopotam"):
    val api = KoHttpRequest[F].withUri(uri"http://api.zippopotam.us/US/")
    val expected = """{"country": "United States", "country abbreviation": "US", "post code": "12561", "places": [{"place name": "New Paltz", "longitude": "-74.1092", "latitude": "41.7464", "state": "New York", "state abbreviation": "NY"}]}"""

    nest("decode"):
      nest("JSON"):
        import org.http4s.circe.CirceEntityDecoder.*
        import io.circe.generic.auto.*

        test("class"):
          case class PostCode(
            `post code`: String,
            country: String,
            `country abbreviation`: String,
            places: Seq[Place],
          )
          case class Place(
            `place name`: String,
            state: String,
            `state abbreviation`: String,
            longitude: Double,
            latitude: Double,
          )

          for
            respBody <- client.run(api.GET(path"12561").acceptJson[PostCode]).decodeSuccess.toBody
          yield
            assertEquals(respBody, circe.parser.decode[PostCode](expected).toOption.get)

        test("AST"):
          for
            respBody <- client.run(api.GET(path"12561").acceptJson[circe.Json]).decodeSuccess.toBody
          yield
            assertEquals(respBody, circe.parser.parse(expected).toOption.get)


      test("String"):
        for
          respBody <- client.run(api.GET(path"12561").acceptString).decodeSuccess.toBody
        yield
          assertEquals(respBody, expected)

      test("Binary"):
        for
          respBody <- client.run(api.GET(path"12561").acceptByteVector).decodeSuccess.toBody
          logged <- printLoggedDebug
        yield
          assertEquals(respBody.toSeq, expected.getBytes(StandardCharsets.UTF_8).toSeq)

    test("404"):
      for
        ex <- client.run(api.GET(path"hello")).decodeSuccess.toBody
            .intercept[KoHttpResponse.UnexpectedStatusAndEntity]
      yield
        assertEquals(ex.status, Status.NotFound)
        assertEquals(ex.entity, "{}")
*/

  nest("encode"):
    nest("JSON"):
      case class Foo(bar: String)
      import org.http4s.circe.CirceEntityEncoder.*
      import io.circe.generic.auto.*

      test("class"):
        val req = KoHttpRequest[F].withBody(Foo("bar0"))
        assertEquals(req.headers.get[`Content-Type`],
            Some(`Content-Type`(MediaType.application.json)))

      test("AST"):
        val req = KoHttpRequest[F].withBody(Foo("bar0").asJson)
        assertEquals(req.headers.get[`Content-Type`],
            Some(`Content-Type`(MediaType.application.json)))

    test("UrlForm"):
      val req = KoHttpRequest[F].withUrlForm("foo" -> "bar")
      assertEquals(req.headers.get[`Content-Type`],
          Some(`Content-Type`(MediaType.application.`x-www-form-urlencoded`, Charset.`UTF-8`)))

    test("Multipart"):
      val boundary = Boundary("boundary00")
      val req = KoHttpRequest[F].withBody(Multipart(Vector(Part.formData("name00", "value00")), boundary))
      assertEquals(req.headers.get[`Content-Type`],
          Some(`Content-Type`(MediaType.multipartType("form-data", Some(boundary.value)))))

  // https://httpbun.org/
  // 2025-12 switched from httpbin.org since it responds with "503 Service Temporarily Unavailable" half the time
  // 2025-12 httpbun.org can be accessed from Chrome, but keep producing TLS errors
/*
  nest("httpbin.org"):
    val api = KoHttpRequest[F].withUri(uri"https://httpbun.org/")

    test("405"):
      for
        ex <- client.run(api.GET(path"post")).decodeSuccess.toBody
            .intercept[KoHttpResponse.UnexpectedStatusAndEntity]
      yield
        assertEquals(ex.status, Status.MethodNotAllowed)
        assert(ex.entity.startsWith("<!DOCTYPE HTML PUBLIC "))


    test("503"):
      for
        ex <- client.run(api.GET(path"status/503")).decodeSuccess.toBody
            .intercept[KoHttpResponse.UnexpectedStatusAndEntity]
      yield
        assertEquals(ex.status, Status.ServiceUnavailable)
        assertEquals(ex.entity, "")
*/

  // https://jsonplaceholder.typicode.com/
  nest("jsonplaceholder.typicode.com"):
    val api = KoHttpRequest[F].withUri(uri"https://jsonplaceholder.typicode.com/")

    import org.http4s.circe.CirceEntityDecoder.*
    import io.circe.generic.auto.*

    case class Todo(
      userId: Int,
      id: Int,
      title: String,
      completed: Boolean,
    )

    test("todos/1"):
      for
        respBody <- client.run(api.GET(path"todos/1").acceptJson[Todo]).decodeSuccess.toBody
      yield
        assertEquals(respBody, Todo(1, 1, "delectus aut autem", false))
