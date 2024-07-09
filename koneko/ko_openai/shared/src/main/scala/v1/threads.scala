package jp.ukiba.koneko
package ko_openai

import jp.ukiba.koneko.ko_http4s.client.KoHttpClient
import jp.ukiba.koneko.ko_http4s.client.syntax.all.*

import org.http4s.circe.CirceEntityDecoder.*
import org.http4s.circe.CirceEntityEncoder.*
import org.http4s.Header
import org.typelevel.log4cats.Logger
import cats.effect.Temporal
import cats.syntax.all.*
import io.circe, circe.syntax.*, circe.generic.auto.*, circe.{Encoder, Decoder}
import org.typelevel.ci.CIStringSyntax

case class thread(
  id: String,
  //`object`: String, // always "thread"
  created_at: Int,
  //tool_resources,
  metadata: Map[String, String],
)

case class message(
  id: String,
  //`object`: String, // always "thread.message"
  created_at: Int,
  thread_id: String,
  status: Option[String], // in_progress, incomplete, completed // 2024-07-12 missing from POST messages
  incomplete_details: Option[String],
  completed_at: Option[Int],
  incomplete_at: Option[Int],
  role: String, // user, assistant
  content: Seq[message.content],
  assistant_id: Option[String],
  run_id: Option[String],
  //attachments: Seq[],
  metadata: Map[String, String],
)

object message:
  sealed trait content:
    def `type`: String

  object content:
    case class image_file(`type`: String, image_file: _image_file.image_file) extends content
    object _image_file: // Scala 3.6.3: named differently to avoid Cyclic reference involving val function
      case class image_file(
        file_id: String,
        detail: Option[String],
      )

    case class image_url(`type`: String, image_url: _image_url.image_url) extends content
    object _image_url: // Scala 3.6.3: named differently to avoid Cyclic reference involving val function
      case class image_url(
        url: String,
        detail: Option[String],
      )

    case class text(`type`: String, text: _text.text) extends content
    object _text: // Scala 3.6.3: named differently to avoid Cyclic reference involving val function
      case class text(
        value: String,
        //annotations: Seq[],
      )

    case class refusal(`type`: String, refusal: String) extends content

    given Encoder[content] = Encoder.instance:
      case x: image_file => x.asJson
      case x: image_url  => x.asJson
      case x: text       => x.asJson
      case x: refusal    => x.asJson

    given Decoder[content] = List[Decoder[content]](
        Decoder[image_file].widen,
        Decoder[image_url ].widen,
        Decoder[text      ].widen,
        Decoder[refusal   ].widen,
      ).reduceLeft(_ or _)
/*
    given Decoder[content] = new Decoder[content]:
      final def apply(cur: HCursor): Decoder.Result[content] =
        cur.downField("type").as[String].flatMap: x =>
          x match
            case "image_file" => Decoder[image_file].apply(cur)
            case "image_url"  => Decoder[image_url ].apply(cur)
            case "text"       => Decoder[text      ].apply(cur)
            case "refusal"    => Decoder[refusal   ].apply(cur)
*/

case class run(
  id: String,
  //`object`: String, // always "thread.run"
  created_at: Int,
  thread_id: String,
  assistant_id: String,
  status: String, // queued, in_progress, requires_action, cancelling, cancelled, failed, completed, incomplete, expired
  //required_action,
  //last_error,
  expires_at: Option[Int],
  started_at: Option[Int],
  cancelled_at: Option[Int],
  failed_at: Option[Int],
  completed_at: Option[Int],
  //incomplete_details,
  model: String,
  instructions: String,
  //tools,
  metadata: Map[String, String],
  //usage,
  temperature: Option[Double],
  top_p: Option[Double],
  max_prompt_tokens: Option[Int],
  max_completion_tokens: Option[Int],
  //truncation_strategy,
  //tool_choice,
  parallel_tool_calls: Boolean,
  //response_format,
)

object threads:
  // Create thread
  object POST:
    def apply[F[_]: Temporal](http: KoHttpClient[F, ?])(req: Request)(using Logger[F]): F[Response] =
        http.POST(s"threads")
            .withBody(req.asJson.deepDropNullValues)
            .withHeader(Header.Raw(ci"OpenAI-Beta", "assistants=v2"))
            .acceptJson[Response].run.decodeSuccess.toBody

    case class Request(
      messages: Seq[String] = Nil,
      tool_resources: Option[tool_resources] = None,
      metadata: Map[String, String] = Map.empty,
    )

    type Response = thread

/*
$ curl https://api.openai.com/v1/threads \
  -H "Content-Type: application/json" \
  -H "Authorization: Bearer $OPENAI_API_KEY" \
  -H "OpenAI-Beta: assistants=v2" \
  -d '{}'
{
  "id": "thread_aNEt32Nvi6jCdWg7P8bLlcWu",
  "object": "thread",
  "created_at": 1720749613,
  "metadata": {},
  "tool_resources": {}
}
*/

  // List threads
  object GET:
    // 2024-07-12 401 Unauthorized: Your request to GET /v1/threads must be made with a session key (that is, it can only be made from the browser)
    def apply[F[_]: Temporal](http: KoHttpClient[F, ?])(req: Request)(using Logger[F]): F[Response] =
        http.GET(s"threads") // FIXME use req
            .withHeader(Header.Raw(ci"OpenAI-Beta", "assistants=v2"))
            .acceptJson[Response].run.decodeSuccess.toBody

    case class Request(
    )

    type Response = ObjectListResponse[thread]

  object WithIdTypes:
    object messages:
      object POST:
        case class Request(
          role: String, // user, assistant
          content: /*String |*/ Seq[Request.content],
          //attachments: Seq[],
          metadata: Map[String, String] = Map.empty,
        )
        object Request:
          sealed trait content:
            def `type`: String

          // https://platform.openai.com/docs/api-reference/assistants/object
          object content:
            case class image_file(`type`: String, image_file: _image_file.image_file) extends content
            object image_file:
              def apply(file_id: String, detail: Option[String] = None): image_file =
                image_file("image_file", _image_file.image_file(file_id, detail))
            object _image_file: // Scala 3.6.3: named differently to avoid Cyclic reference involving val function
              case class image_file(
                file_id: String,
                detail: Option[String],
              )

            case class image_url(`type`: String, image_url: _image_url.image_url) extends content
            object image_url:
              def apply(url: String, detail: Option[String] = None): image_url =
                image_url("image_url", _image_url.image_url(url, detail))
            object _image_url: // Scala 3.6.3: named differently to avoid Cyclic reference involving val function
              case class image_url(
                url: String,
                detail: Option[String],
              )

            case class text(`type`: String, text: String) extends content
            object text:
              def apply(text: String): text =
                new text("text", text)

            given Encoder[content] = Encoder.instance:
              case x: image_file => x.asJson
              case x: image_url  => x.asJson
              case x: text       => x.asJson

            given Decoder[content] = List[Decoder[content]](
                Decoder[image_file].widen,
                Decoder[image_url ].widen,
                Decoder[text      ].widen,
              ).reduceLeft(_ or _)

        type Response = message

  class WithId(thread_id: String):
    object messages:
      // Create message
      object POST:
        def apply[F[_]: Temporal](http: KoHttpClient[F, ?])(req: Request)(using Logger[F]): F[Response] =
            http.POST(s"threads/$thread_id/messages")
                .withBody(req.asJson.deepDropNullValues)
                .withHeader(Header.Raw(ci"OpenAI-Beta", "assistants=v2"))
                .acceptJson[Response].run.decodeSuccess.toBody

        export WithIdTypes.messages.POST.*

      // List messages
      object GET:
        def apply[F[_]: Temporal](http: KoHttpClient[F, ?])(req: Request)(using Logger[F]): F[Response] =
            http.GET(s"threads/$thread_id/messages") // FIXME use req
                .withHeader(Header.Raw(ci"OpenAI-Beta", "assistants=v2"))
                .acceptJson[Response].run.decodeSuccess.toBody

        case class Request(
          limit : Option[Int] = None,
          order : Option[String] = None,
          after : Option[String] = None,
          before: Option[String] = None,
          run_id: Option[String] = None,
        )

        type Response = ObjectListResponse[message]

    object runs:
      // Create run
      object POST:
        def apply[F[_]: Temporal](http: KoHttpClient[F, ?])(req: Request)(using Logger[F]): F[Response] =
            http.POST(s"threads/$thread_id/runs")
                .withBody(req.asJson.deepDropNullValues)
                .withHeader(Header.Raw(ci"OpenAI-Beta", "assistants=v2"))
                .acceptJson[Response].run.decodeSuccess.toBody

        case class Request(
          assistant_id: String,
          model: Option[String] = None, // override the model of assistant
          instructions: Option[String] = None, // override that of assistant
          additional_instructions: Option[String] = None,
          //additional_messages: Seq[messages.POST.Request] = Nil,
          tools: Option[Seq[tool]] = None, // override that of assistant
          metadata: Map[String, String] = Map.empty,
          temperature: Option[Double] = None,
          top_p: Option[Double] = None,
          //stream,
          max_prompt_tokens: Option[Int] = None,
          max_completion_tokens: Option[Int] = None,
          //truncation_strategy,
          tool_choice: Option[String /*| tool*/] = None, // TODO
          parallel_tool_calls: Option[Boolean] = None,
          //response_format,
        )

        type Response = run

      class WithId(run_id: String):
        // Retrieve run
        object GET:
          def apply[F[_]: Temporal](http: KoHttpClient[F, ?])(using Logger[F]): F[Response] =
              http.GET(s"threads/$thread_id/runs/$run_id")
                  .withHeader(Header.Raw(ci"OpenAI-Beta", "assistants=v2"))
                  .acceptJson[Response].run.decodeSuccess.toBody

          type Response = run

