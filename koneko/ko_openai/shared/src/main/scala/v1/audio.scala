package jp.ukiba.koneko
package ko_openai

import jp.ukiba.koneko.ko_http4s.client.KoHttpClient
import jp.ukiba.koneko.ko_http4s.client.syntax.all.*

import org.http4s.multipart.{Multiparts, Part}
import org.http4s.circe.CirceEntityDecoder.*
import org.http4s.circe.CirceEntityEncoder.*
import fs2.io.file.{Path, Files}
import org.typelevel.log4cats.Logger
import cats.effect.Async
import cats.syntax.all.*
import io.circe, circe.syntax.*, circe.generic.auto.*

object audio:
  object transcriptions:
    object POST:
      def apply[F[_]: Async: Files](http: KoHttpClient[F, ?])(req: Request)(using Logger[F]): F[Response] =
        var parts = Vector.empty[Part[F]]
        parts :+= Part.fileData[F]("file", req.file)
        parts :+= Part.formData("model", req.model)
        for (language <- req.language)
          parts :+= Part.formData("language", language)
        for (temperature <- req.temperature)
          parts :+= Part.formData("temperature", temperature.toString)

        for
          multiparts <- Multiparts.forSync
          multipart <- multiparts.multipart(parts: Vector[Part[F]])
          resp <- http.POST(s"audio/transcriptions")
              .withBody(multipart)
              .acceptJson[Response].run.decodeSuccess.toBody
        yield resp

      case class Request(
        file: Path /*| Array[Byte]*/,
        model: String,
        language: Option[String] = None,
        temperature: Option[Double] = None,
      )

      case class Response(
        text: String,
      )
