package jp.ukiba.koneko
package ko_azure

import jp.ukiba.koneko.ko_http4s.client.KoHttpClient
import jp.ukiba.koneko.ko_http4s.client.syntax.all.*

import org.http4s.circe.CirceEntityDecoder.*
import org.http4s.circe.CirceEntityEncoder.*
import org.http4s.{Headers, Uri}
import org.typelevel.log4cats.Logger
import cats.effect.Temporal
import cats.syntax.all.*
import io.circe, circe.syntax.*, circe.generic.auto.*
import org.typelevel.ci.{CIStringSyntax, CIString}

import java.time.Instant

// FIXME this is not part of ko_openai source tree
object documentintelligence:
  extension (headers: Headers)
    def singleValueOrThrow(key: CIString): String = headers.get(key) match
      case Some(nel) =>
        if nel.tail.isEmpty then
          nel.head.value
        else
          throw Exception(s"Multiple header values: $key: ${nel.toList.map(_.value).mkString(", ")}")
      case None => throw Exception(s"No header: $key")

  object documentModels:
    object GET:
      // https://learn.microsoft.com/en-us/rest/api/aiservices/document-models/list-models?view=rest-aiservices-v4.0%20(2024-07-31-preview)&tabs=HTTP
      def apply[F[_]: Temporal](http: KoHttpClient[F, ?])(req: Request)(using Logger[F]): F[Response] =
          http.GET(s"documentintelligence/documentModels")
              .withBody(req.asJson.deepDropNullValues)
              .acceptJson[Response].run.decodeSuccess.toBody

      case class Request()

      case class Response(
        value: Seq[Response.Value],
        // 2024-09-02: nextLink is missing
      )

      object Response:
        case class Value(
          modelId: String, // 2024-09-02: there are "prebuilt-layout", and "prebuilt-read"
          createdDateTime: Instant,
          apiVersion: String,
          description: String, // 2024-09-02: the last element, and always exist
        )

  class documentModels(modelId: String):
    object analyze:
      object POST:
        /*
          Response(status=202, httpVersion=HTTP/1.1, headers=Headers(Content-Length: 0, Operation-Location: https://sysart-ai2024-doc-intell.cognitiveservices.azure.com/documentintelligence/documentModels/prebuilt-read/analyzeResults/918ef0bb-b97d-4931-b4d5-a4c271317226?api-version=2024-07-31-preview, x-envoy-upstream-service-time: 60, apim-request-id: 918ef0bb-b97d-4931-b4d5-a4c271317226, Strict-Transport-Security: max-age=31536000; includeSubDomains; preload, x-content-type-options: nosniff, x-ms-region: East US, Date: Mon, 02 Sep 2024 02:48:08 GMT), entity=Entity.Empty)
        */
        def apply[F[_]: Temporal](http: KoHttpClient[F, ?])(req: Request)(using Logger[F]): F[Response] =
            http.POST(s"documentintelligence/documentModels/$modelId:analyze")
                .withBody(req.asJson.deepDropNullValues)
                .run.expectStatus(_.code == 202).resource.use: resp => // no response body
                  Response(
                    Uri.fromString(resp.headers.singleValueOrThrow(ci"Operation-Location")).toTry.get,
                    resp.headers.singleValueOrThrow(ci"apim-request-id"),
                  ).pure[F] // TODO is pure correct (defer is not available with Temporal)

        case class Request(
          base64Source: String,
        )

        case class Response(
          `Operation-Location`: Uri,
          `apim-request-id`: String,
        )

/*
        documentModels <- ko_azure.documentintelligence.documentModels.pure[F]
        listModelReq = documentModels.GET.Request()
        _ <- log.debug(s"listModelReq = $listModelReq")
        listModelResp <- documentModels.GET(docIntellHttp)(listModelReq)
        _ <- log.debug(s"listModelResp = $listModelResp")
*/

    object analyzeResults:
      object GET:
        // https://learn.microsoft.com/en-us/rest/api/aiservices/document-models/get-analyze-result-pdf?view=rest-aiservices-v4.0%20(2024-07-31-preview)&tabs=HTTP
        def apply[F[_]: Temporal](http: KoHttpClient[F, ?])(uri: Uri)(using Logger[F]): F[Response] =
            http.GET.withUri(uri) // uri is returned from analyze.POST
                .acceptJson[Response].run.decodeSuccess.toBody

        case class Response(
          status: String,
          createdDateTime: Instant,
          lastUpdatedDateTime: Instant,
          analyzeResult: Option[Response.AnalyzeResult], // missing when status is "running"
        )

        object Response:
          case class AnalyzeResult(
            apiVersion: String,
            modelId: String,
            stringIndexType: String,
            content: String,
            // pages
            // paragraphs
            // styles
            // contentFormat
          )
