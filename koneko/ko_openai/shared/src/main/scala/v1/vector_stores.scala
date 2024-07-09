package jp.ukiba.koneko
package ko_openai

import jp.ukiba.koneko.ko_http4s.client.KoHttpClient
import jp.ukiba.koneko.ko_http4s.client.syntax.all.*

import org.http4s.circe.CirceEntityDecoder.*
import org.http4s.circe.CirceEntityEncoder.*
import org.http4s.Header
import org.typelevel.log4cats.Logger
import cats.effect.Temporal
import io.circe, circe.syntax.*, circe.generic.auto.*
import org.typelevel.ci.CIStringSyntax

case class vector_store(
  id: String,
  //`object`: String, // always "vector_store"
  created_at: Int,
  name: Option[String],
  usage_bytes: Int,
  file_counts: vector_store.file_counts,
  status: String, // expired, in_progress, completed
  expires_after: Option[vector_store.expires_after],
  expires_at    : Option[Int],
  last_active_at: Option[Int],
  metadata: Map[String, String],
)

object vector_store:
  case class file_counts(
    in_progress: Int,
    completed  : Int,
    failed     : Int,
    cancelled  : Int,
    total      : Int,
  )

  case class expires_after(
    anchor: String,
    days: Int,
  )

case class vector_store_file(
  id: String,
  //`object`: String, // always "vector_store.file"
  usage_bytes: Int,
  created_at: Int,
  vector_store_id: String,
  status: String, // in_progress, completed, cancelled, failed
  //last_error: Option[],
  //chunking_strategy,
)

object vector_stores:
  // Create vector store
  object POST:
    def apply[F[_]: Temporal](http: KoHttpClient[F, ?])(req: Request)(using Logger[F]): F[Response] =
        http.POST(s"vector_stores")
            .withBody(req.asJson.deepDropNullValues)
            .withHeader(Header.Raw(ci"OpenAI-Beta", "assistants=v2")) // 2024-07-12 required
            .acceptJson[Response].run.decodeSuccess.toBody

    case class Request( // 2024-07-12 succeeded with the empty request
      file_ids: Option[Seq[String]] = None,
      name: Option[String] = None,
      expires_after: Option[vector_store.expires_after] = None,
      //chunking_strategy,
      metadata: Map[String, String] = Map.empty,
    )

    type Response = vector_store

  // List vector stores
  object GET:
    // 2024-07-12 returns empty data even if https://chatgpt.com/gpts/mine is non-empty
    def apply[F[_]: Temporal](http: KoHttpClient[F, ?])(req: Request)(using Logger[F]): F[Response] =
        http.GET(s"vector_stores") // FIXME use req
            .withHeader(Header.Raw(ci"OpenAI-Beta", "assistants=v2")) // 2024-07-12 required
            .acceptJson[Response].run.decodeSuccess.toBody

    case class Request(
      limit : Option[Int] = None,
      order : Option[String] = None,
      after : Option[String] = None,
      before: Option[String] = None,
    )

    type Response = ObjectListResponse[vector_store]

    /*
      $ curl https://api.openai.com/v1/vector_stores \
        -H "Authorization: Bearer $OPENAI_API_KEY" \
        -H "Content-Type: application/json" \
        -H "OpenAI-Beta: assistants=v2"
      {
        "object": "list",
        "data": [
          {
            "id": "vs_NEZ98yWbCR9DCgnkAjKJjUj0",
            "object": "vector_store",
            "name": "sysart-ai2024",
            "status": "completed",
            "usage_bytes": 24524,
            "created_at": 1720740332,
            "file_counts": {
              "in_progress": 0,
              "completed": 2,
              "failed": 0,
              "cancelled": 0,
              "total": 2
            },
            "metadata": {},
            "expires_after": null,
            "expires_at": null,
            "last_active_at": 1720759763
          }
        ],
        "first_id": "vs_NEZ98yWbCR9DCgnkAjKJjUj0",
        "last_id": "vs_NEZ98yWbCR9DCgnkAjKJjUj0",
        "has_more": false
      }
    */

  class WithId(vector_store_id: String):
    // Retrieve vector store
    object GET:
      def apply[F[_]: Temporal](http: KoHttpClient[F, ?])(using Logger[F]): F[Response] =
          http.GET(s"vector_stores/$vector_store_id")
              .withHeader(Header.Raw(ci"OpenAI-Beta", "assistants=v2"))
              .acceptJson[Response].run.decodeSuccess.toBody

      type Response = vector_store

    // Modify vector store
    object POST:
      def apply[F[_]: Temporal](http: KoHttpClient[F, ?])(req: Request)(using Logger[F]): F[Response] =
          http.POST(s"vector_stores/$vector_store_id")
              .withBody(req.asJson.deepDropNullValues)
              .withHeader(Header.Raw(ci"OpenAI-Beta", "assistants=v2"))
              .acceptJson[Response].run.decodeSuccess.toBody

      case class Request(
        name: Option[String] = None,
        expires_after: Option[vector_store.expires_after] = None,
        metadata: Map[String, String] = Map.empty,
      )

      type Response = vector_store

    // Delete vector store
    object DELETE:
      def apply[F[_]: Temporal](http: KoHttpClient[F, ?])(using Logger[F]): F[Response] =
          http.DELETE(s"vector_stores/$vector_store_id")
              .withHeader(Header.Raw(ci"OpenAI-Beta", "assistants=v2"))
              .acceptJson[Response].run.decodeSuccess.toBody

      type Response = DeleteStatus

    object files:
      // List vector store files
      object GET:
        // 2024-07-12 empty even if https://chatgpt.com/gpts/mine is non-empty
        def apply[F[_]: Temporal](http: KoHttpClient[F, ?])(req: Request)(using Logger[F]): F[Response] =
            http.GET(s"vector_stores/$vector_store_id/files") // FIXME use req
                .withHeader(Header.Raw(ci"OpenAI-Beta", "assistants=v2")) // 2024-07-12 required
                .acceptJson[Response].run.decodeSuccess.toBody

        case class Request(
          limit : Option[Int] = None,
          order : Option[String] = None,
          after : Option[String] = None,
          before: Option[String] = None,
          filter: Option[String] = None,
        )

        type Response = ObjectListResponse[vector_store_file]

/*
$ curl https://api.openai.com/v1/vector_stores/vs_NEZ98yWbCR9DCgnkAjKJjUj0/files \
    -H "Authorization: Bearer $OPENAI_API_KEY" \
    -H "Content-Type: application/json" \
    -H "OpenAI-Beta: assistants=v2" \
    -d '{
      "file_id": "file-eE3w2OlnHWvcAfB3YHo3d2qR"
    }'
{
  "id": "file-eE3w2OlnHWvcAfB3YHo3d2qR",
  "object": "vector_store.file",
  "usage_bytes": 0,
  "created_at": 1720743176,
  "vector_store_id": "vs_NEZ98yWbCR9DCgnkAjKJjUj0",
  "status": "in_progress",
  "last_error": null,
  "chunking_strategy": {
    "type": "static",
    "static": {
      "max_chunk_size_tokens": 800,
      "chunk_overlap_tokens": 400
    }
  }
}
*/
/*
$ curl https://api.openai.com/v1/vector_stores/vs_NEZ98yWbCR9DCgnkAjKJjUj0/files \\
    -H "Authorization: Bearer $OPENAI_API_KEY" \
    -H "Content-Type: application/json" \
    -H "OpenAI-Beta: assistants=v2" \
    -d '{
      "file_id": "file-vLfZEHSwYwPQ3YqHo16Pz57C"
    }'
{
  "id": "file-vLfZEHSwYwPQ3YqHo16Pz57C",
  "object": "vector_store.file",
  "usage_bytes": 0,
  "created_at": 1720743215,
  "vector_store_id": "vs_NEZ98yWbCR9DCgnkAjKJjUj0",
  "status": "in_progress",
  "last_error": null,
  "chunking_strategy": {
    "type": "static",
    "static": {
      "max_chunk_size_tokens": 800,
      "chunk_overlap_tokens": 400
    }
  }
}
*/

      // Create vector store file
      object POST:
        def apply[F[_]: Temporal](http: KoHttpClient[F, ?])(req: Request)(using Logger[F]): F[Response] =
            http.POST(s"vector_stores/$vector_store_id/files")
                .withBody(req.asJson.deepDropNullValues)
                .withHeader(Header.Raw(ci"OpenAI-Beta", "assistants=v2")) // 2024-07-12 required
                .acceptJson[Response].run.decodeSuccess.toBody

        case class Request( // 2024-07-12 succeeded with the empty request
          file_id: String,
          //chunking_strategy,
        )

        type Response = vector_store

