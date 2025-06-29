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

  // https://api.zippopotam.us/
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

  // https://httpbin.org/
  nest("httpbin.org"):
    val api = KoHttpRequest[F].withUri(uri"https://httpbin.org/")

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
