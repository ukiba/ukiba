package jp.ukiba.koneko
package ko_openai

import jp.ukiba.koneko.ko_http4s.client.KoHttpClient
import jp.ukiba.koneko.ko_http4s.client.syntax.all.*

import org.http4s.circe.CirceEntityDecoder.*
import org.http4s.circe.CirceEntityEncoder.*
import org.typelevel.log4cats.Logger
import cats.effect.Temporal
import io.circe, circe.syntax.*, circe.generic.auto.*

object chat:
  object completions:
    object POST:
      // https://api.openai.com/v1/chat/completions
      def apply[F[_]: Temporal](http: KoHttpClient[F, ?])(req: Request)(using Logger[F]): F[Response] =
          http.POST(s"chat/completions")
              .withBody(req.asJson.deepDropNullValues)
              .acceptJson[Response].run.decodeSuccess.toBody

      case class Request(
        messages: Seq[Request.Message],
        model: String,
        frequency_penalty: Option[Double] = None,
        //logit_bias
        //logprobs: Boolean = false,
        //top_logprobs: Int = 0,
        max_tokens: Option[Int] = None,
        n: Option[Int] = None,
        presence_penalty: Option[Double] = None,
        //response_format
        seed: Option[Int] = None,
        service_tier: Option[String] = None,
        //stop
        stream: Option[Boolean] = None,
        //stream_options
        temperature: Option[Double] = None,
        top_p: Option[Double] = None,
        //tools
        //tool_choice
        parallel_tool_calls: Option[Boolean] = None,
        user: Option[String] = None,
        //function_call (deeprecated)
        //functions (deprecated)
      )

      object Request:
        case class Message(
          role: String,
          content: String,
        )

      case class Response(
        id: String,
        choices: Seq[Response.Choice],
        created: Int,
        model: String,
        service_tier: Option[String],
        system_fingerprint: Option[String], // JSON can be null
        `object`: String, // always "chat.completion"
        usage: Response.Usage,
      )

      object Response:
        case class Choice(
          index: Int,
          message: Choice.Message,
          //logprobs
          //finish_reason
        )
        object Choice:
          case class Message(
            role: String,
            content: String,
          )

        case class Usage(
          prompt_tokens: Int,
          completion_tokens: Int,
          total_tokens: Int,
        )
