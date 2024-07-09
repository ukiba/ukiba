package jp.ukiba.koneko
package ko_openai

import jp.ukiba.koneko.ko_http4s.client.KoHttpClient
import jp.ukiba.koneko.ko_http4s.client.syntax.all.*

import org.http4s.multipart.{Multiparts, Part}
import org.http4s.circe.CirceEntityDecoder.*
import org.http4s.circe.CirceEntityEncoder.*
import org.http4s.Entity
import org.typelevel.log4cats.Logger
import cats.effect.{Temporal, Async}
import cats.syntax.all.*
import io.circe, circe.syntax.*, circe.generic.auto.*
import scodec.bits.ByteVector

import java.time.Instant

case class file(
  `object`: String, // always "file" (not documented)
  id: String,
  purpose: String, // assistants, assistants_output, batch, batch_output, fine-tune, fine-tune-results, vision
  filename: String,
  bytes: Long,
  created_at: Int,
  //status // deprecated
  //status_details // deprecated
):
  def name: String = filename
  def size: Long = bytes
  def created: Instant = Instant.ofEpochSecond(created_at)

object files:
  // Upload file
  object POST:
    def apply[F[_]: Async](http: KoHttpClient[F, ?])(req: Request[F])(using Logger[F]): F[Response] =
      for
        purposePart <- Part.formData("purpose", req.purpose).pure
        filePart <- Part.fileData("file", req.fileName, req.fileContent).pure

        multiparts <- Multiparts.forSync // TODO reuse
        multipart <- multiparts.multipart(Vector(purposePart, filePart))

        resp <- http.POST(s"files")
            .withBody(multipart)
            .acceptJson[Response].run.decodeSuccess.toBody
      yield resp

    case class Request[F[_]](
      fileName: String,
      fileContent: Entity[F],
      purpose: String,
    )

    type Response = file

/*
$ curl https://api.openai.com/v1/files \
  -H "Authorization: Bearer $OPENAI_API_KEY" \
  -F purpose="assistants" \
  -F file="@2024-05-sample2.docx"
{
  "object": "file",
  "id": "file-eE3w2OlnHWvcAfB3YHo3d2qR",
  "purpose": "assistants",
  "filename": "2024-05-sample2.docx",
  "bytes": 24257,
  "created_at": 1720742570,
  "status": "processed",
  "status_details": null
}

$ url https://api.openai.com/v1/files \
  -H "Authorization: Bearer $OPENAI_API_KEY" \
  -F purpose="assistants" \
  -F file="@2024-05-sample2.pptx"
{
  "object": "file",
  "id": "file-vLfZEHSwYwPQ3YqHo16Pz57C",
  "purpose": "assistants",
  "filename": "2024-05-sample2.pptx",
  "bytes": 357130,
  "created_at": 1720742597,
  "status": "processed",
  "status_details": null
}

$ curl https://api.openai.com/v1/files \
  -H "Authorization: Bearer $OPENAI_API_KEY" \
  -F purpose="assistants" \
  -F file="@2024-05-sample2.xlsx"
{
  "object": "file",
  "id": "file-WtO3mRjAMJ2jPVL74cc851lV",
  "purpose": "assistants",
  "filename": "2024-05-sample2.xlsx",
  "bytes": 317202,
  "created_at": 1721695935,
  "status": "processed",
  "status_details": null
}
*/


/*
$ curl https://api.openai.com/v1/files \
  -H "Authorization: Bearer $OPENAI_API_KEY" \
  -F purpose="assistants" \
  -F file="@2024-05-sample2.docx.pdf"
{
  "object": "file",
  "id": "file-Zf5YS2chdrADxkT3iLBxMkJe",
  "purpose": "assistants",
  "filename": "2024-05-sample2.docx.pdf",
  "bytes": 150842,
  "created_at": 1721703035,
  "status": "processed",
  "status_details": null
}

$ curl https://api.openai.com/v1/files \
  -H "Authorization: Bearer $OPENAI_API_KEY" \
  -F purpose="assistants" \
  -F file="@2024-05-sample2.pptx.pdf"
{
  "object": "file",
  "id": "file-xOaEHDxdW8KJHVcpumjmEMPS",
  "purpose": "assistants",
  "filename": "2024-05-sample2.pptx.pdf",
  "bytes": 1308480,
  "created_at": 1721703058,
  "status": "processed",
  "status_details": null
}

$ $ curl https://api.openai.com/v1/files \
  -H "Authorization: Bearer $OPENAI_API_KEY" \
  -F purpose="assistants" \
  -F file="@2024-05-sample2.xlsm.pdf"
{
  "object": "file",
  "id": "file-A8rpZ0X6qSeyTw5LYG3W43To",
  "purpose": "assistants",
  "filename": "2024-05-sample2.xlsm.pdf",
  "bytes": 1095273,
  "created_at": 1721703081,
  "status": "processed",
  "status_details": null
}
*/

  // List files
  object GET:
    def apply[F[_]: Temporal](http: KoHttpClient[F, ?])(req: Request)(using Logger[F]): F[Response] =
        http.GET(s"files") // FIXME use req
            .acceptJson[Response].run.decodeSuccess.toBody

    case class Request(
      purpose: Option[String] = None,
      limit: Option[Int] = None,
      order: Option[String] = None,
      after: Option[String] = None,
    )

    type Response = ObjectListResponse[file]
