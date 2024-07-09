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

case class assistant(
  id: String,
  `object`: String, // always "assistant"
  created_at: Int,
  name        : Option[String],
  description : Option[String],
  model: String,
  instructions: Option[String],
  tools: Seq[tool],
  top_p: Option[Double],
  temperature: Option[Double],
  tool_resources: Option[tool_resources],
  //metadata
  response_format: String | response_format,
)

object assistants:
  // Create assistant
  object POST:
    def apply[F[_]: Temporal](http: KoHttpClient[F, ?])(req: Request)(using Logger[F]): F[Response] =
        http.POST(s"assistants")
            .withBody(req.asJson.deepDropNullValues)
            .withHeader(Header.Raw(ci"OpenAI-Beta", "assistants=v2"))
            .acceptJson[Response].run.decodeSuccess.toBody

    case class Request(
      model: String,
      name        : Option[String],
      description : Option[String],
      instructions: Option[String],
      tools: Seq[tool],
      //tool_resources,
      metadata: Map[String, String],
      temperature: Option[Double],
      top_p: Option[Double],
      //response_format,
    )

    type Response = assistant

/*
$ curl "https://api.openai.com/v1/assistants" \
  -H "Content-Type: application/json" \
  -H "Authorization: Bearer $OPENAI_API_KEY" \
  -H "OpenAI-Beta: assistants=v2" \
  -d '{
    "instructions": "あなたは資料整理を手伝います。箇条書きで答えてください。",
    "name": "sysart-ai2024",
    "tools": [{"type": "file_search"}],
    "tool_resources": {
      "file_search": {
        "vector_store_ids": ["vs_NEZ98yWbCR9DCgnkAjKJjUj0"]
      }
    },
    "model": "gpt-4o"
  }'
{
  "id": "asst_1KKdXA7G3Vo0jKZ1yWkuIHOO",
  "object": "assistant",
  "created_at": 1720745556,
  "name": "sysart-ai2024",
  "description": null,
  "model": "gpt-4o",
  "instructions": "あなたは資料整理を手伝います。箇条書きで答えてください。",
  "tools": [
    {
      "type": "file_search"
    }
  ],
  "top_p": 1.0,
  "temperature": 1.0,
  "tool_resources": {
    "file_search": {
      "vector_store_ids": [
        "vs_NEZ98yWbCR9DCgnkAjKJjUj0"
      ]
    }
  },
  "metadata": {},
  "response_format": "auto"
}
*/

/*
$ curl "https://api.openai.com/v1/assistants" \
  -H "Content-Type: application/json" \
  -H "Authorization: Bearer $OPENAI_API_KEY" \
  -H "OpenAI-Beta: assistants=v2" \
  -d '{
    "name": "sysart-ai2024",
    "tools": [{"type": "file_search"}],
    "model": "gpt-4o"
  }'
{
  "id": "asst_i3B30yUFB6uDv4emIWtGTVcC",
  "object": "assistant",
  "created_at": 1721707197,
  "name": "sysart-ai2024",
  "description": null,
  "model": "gpt-4o",
  "instructions": null,
  "tools": [
    {
      "type": "file_search"
    }
  ],
  "top_p": 1.0,
  "temperature": 1.0,
  "tool_resources": {
    "file_search": {
      "vector_store_ids": []
    }
  },
  "metadata": {},
  "response_format": "auto"
}
*/

/*
curl https://api.openai.com/v1/assistants/asst_zt2e2179hf1flGwIfocGIJrO \
  -H "Content-Type: application/json" \
  -H "Authorization: Bearer $OPENAI_API_KEY" \
  -H "OpenAI-Beta: assistants=v2" \
  -X DELETE
{
  "id": "asst_RLVGaMaval1eNoKiLLvYEIZK",
  "object": "assistant.deleted",
  "deleted": true
}
*/

  // List assistants
  object GET:
    def apply[F[_]: Temporal](http: KoHttpClient[F, ?])(req: Request)(using Logger[F]): F[Response] =
        http.GET(s"assistants") // FIXME use req
            .withHeader(Header.Raw(ci"OpenAI-Beta", "assistants=v2"))
            .acceptJson[Response].run.decodeSuccess.toBody

    case class Request(
      limit: Option[Int] = None,
      order: Option[String] = None,
      after: Option[String] = None,
      before: Option[String] = None,
    )

    type Response = ObjectListResponse[assistant]
