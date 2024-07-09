# REPL

`server / Test / console`

```
import jp.ukiba.koneko.ko_openai
import jp.ukiba.koneko.ko_http4s.client.KoHttpClient

import org.http4s.ember.client.EmberClientBuilder
import org.http4s.syntax.all.*
import org.typelevel.log4cats.{Logger, LoggerFactory}
import org.typelevel.log4cats.slf4j.Slf4jFactory
import cats.syntax.all.*
import io.circe, circe.syntax.*, circe.generic.auto.*

type F[A] = cats.effect.IO[A]

val program = EmberClientBuilder.default[F].build.use: httpClient =>
  // http4s 0.23.30: moving LoggerFactory before EmberClientBuilder makes
  // the log methods to freeze, even with NoOpFactory
  given LoggerFactory[F] = Slf4jFactory.create[F]
  given log: Logger[F] = LoggerFactory[F].getLogger

  val http = KoHttpClient(httpClient).withUri(uri"https://api.openai.com/v1/")
      .withBearerAuth(sys.env("OPENAI_API_KEY"))
  for
    openaiApi <- ko_openai.files.GET.pure[F]
    openaiApiReq = openaiApi.Request()
    openaiApiResp <- openaiApi(http)(openaiApiReq)
  yield
    println(openaiApiResp.asJson.deepDropNullValues)
program.unsafeRunSync()(using cats.effect.unsafe.implicits.global)
```

# curl

```
curl "https://api.openai.com/v1/files" \
  -H "Authorization: Bearer $OPENAI_API_KEY"
```

```
{
  "object": "list",
  "data": [
    {
      "object": "file",
      "id": "file-QJnpfJF8hBNdrjhNfyGN1s",
      "purpose": "vision",
      "filename": "みかんの木.jpeg",
      "bytes": 937762,
      "created_at": 1738052264,
      "status": "processed",
      "status_details": null
    },
    {
      "object": "file",
      "id": "file-A8rpZ0X6qSeyTw5LYG3W43To",
      "purpose": "assistants",
      "filename": "2024-05-sample2.xlsm.pdf",
      "bytes": 1095273,
      "created_at": 1721703081,
      "status": "processed",
      "status_details": null
    },
    {
      "object": "file",
      "id": "file-xOaEHDxdW8KJHVcpumjmEMPS",
      "purpose": "assistants",
      "filename": "2024-05-sample2.pptx.pdf",
      "bytes": 1308480,
      "created_at": 1721703058,
      "status": "processed",
      "status_details": null
    },
    {
      "object": "file",
      "id": "file-Zf5YS2chdrADxkT3iLBxMkJe",
      "purpose": "assistants",
      "filename": "2024-05-sample2.docx.pdf",
      "bytes": 150842,
      "created_at": 1721703035,
      "status": "processed",
      "status_details": null
    },
    {
      "object": "file",
      "id": "file-WtO3mRjAMJ2jPVL74cc851lV",
      "purpose": "assistants",
      "filename": "2024-05-sample2.xlsx",
      "bytes": 317202,
      "created_at": 1721695935,
      "status": "processed",
      "status_details": null
    },
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
  ],
  "has_more": false,
  "first_id": "file-QJnpfJF8hBNdrjhNfyGN1s",
  "last_id": "file-eE3w2OlnHWvcAfB3YHo3d2qR"
}
```
