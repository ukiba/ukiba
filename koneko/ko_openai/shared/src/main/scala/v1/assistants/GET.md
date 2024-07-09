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
    openaiApi <- ko_openai.assistants.GET.pure[F]
    openaiApiReq = openaiApi.Request()
    openaiApiResp <- openaiApi(http)(openaiApiReq)
  yield
    println(openaiApiResp.asJson.deepDropNullValues)
program.unsafeRunSync()(using cats.effect.unsafe.implicits.global)
```

# curl

```
curl "https://api.openai.com/v1/assistants" \
  -H "Content-Type: application/json" \
  -H "Authorization: Bearer $OPENAI_API_KEY" \
  -H "OpenAI-Beta: assistants=v2"
```

```
{
  "object": "list",
  "data": [
    {
      "id": "asst_zt2e2179hf1flGwIfocGIJrO",
      "object": "assistant",
      "created_at": 1738051882,
      "name": "はたらく細胞で世界を見る",
      "description": null,
      "model": "gpt-4o",
      "instructions": "以下に写真が入力されます。この写真に映る要素をもとに、アニメ『はたらく細胞』に関連する臓器や場面を以下のフォーマットで出力してください。臓器や場面に関しては以下の『更新された臓器一覧』の臓器名と描写・表現の組み合わせを参照してください\n更新された臓器一覧（参照用）\n\t1.\t肺: 駅や交通ハブ\n\t2.\t心臓: ポンプ工場\n\t3.\t腸: フードマーケット・下水処理場\n\t4.\t肝臓: 化学工場・倉庫街\n\t5.\t脳: 管制室・司令センター + ビル群\n\t6.\t皮膚: 城壁や工事現場\n\t7.\t骨髄: 学校・訓練場\n\t8.\tリンパ節: 公園・検問所・集会所\n\t9.\t血管: 道路や高速道路\n\t10.\t胃: 食堂・厨房\n\t11.\t毛細血管: 細い道・末端のルート\n\t12.\t細胞たちの日常: 住宅街・生活空間\n\t13.\t傷んだ組織: 廃墟、古い建物、人が住んでいない建物\nフォーマット\n該当する臓器や描写の例\n1. 臓器名（機能や役割）\n\t•\t関連性: 写真の要素と臓器の働きや特徴を関連付けて説明してください。\n\t•\tキャラクター: 関連するキャラクターを挙げてください\n\t•\tセリフ: 関連するキャラクターがこの写真について語るセリフを挙げてください\n\t•\t該当話数: 関連するエピソードを挙げてください。\n2. 臓器名（機能や役割）\n\t•\t関連性: 同様に、写真の要素と臓器の働きを説明してください。\n\t•\tキャラクター: 関連するキャラクターを挙げてください\n\t•\tセリフ: 関連するキャラクターがこの写真について語るセリフを挙げてください\n\t•\t該当話数: 関連するエピソードを挙げてください。\n特徴的な要素\n\t1.\t要素名: 写真に映る具体的な要素を挙げ、それが臓器や体内の仕組みとどのように関連するか説明してください。\n\t2.\t要素名: 同様に、写真に含まれる他の要素も説明してください。\nまとめ\n写真を総合的に評価し、『更新された臓器一覧』を参照して、どの臓器や場面と関連付けられるかを簡潔に述べてください。\n写真を入力してください。",
      "tools": [],
      "top_p": 1.0,
      "temperature": 1.0,
      "tool_resources": {},
      "metadata": {},
      "response_format": {
        "type": "text"
      }
    },
    {
      "id": "asst_i3B30yUFB6uDv4emIWtGTVcC",
      "object": "assistant",
      "created_at": 1721707197,
      "name": "sysart-dbj-ai-2024-07b",
      "description": null,
      "model": "gpt-4o",
      "instructions": null,
      "tools": [
        {
          "type": "file_search",
          "file_search": {}
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
    },
    {
      "id": "asst_1KKdXA7G3Vo0jKZ1yWkuIHOO",
      "object": "assistant",
      "created_at": 1720745556,
      "name": "sysart-dbj-ai-2024-07-12a",
      "description": null,
      "model": "gpt-4o",
      "instructions": "あなたは資料整理を手伝います。箇条書きで答えてください。",
      "tools": [
        {
          "type": "file_search",
          "file_search": {}
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
    },
    {
      "id": "asst_AdlLkNpYIKMMwTZyrD3tXnBm",
      "object": "assistant",
      "created_at": 1705661063,
      "name": "正雪",
      "description": null,
      "model": "gpt-3.5-turbo-16k",
      "instructions": "江戸時代の烈士",
      "tools": [],
      "top_p": 1.0,
      "temperature": 1.0,
      "tool_resources": {},
      "metadata": {},
      "response_format": "auto"
    }
  ],
  "first_id": "asst_zt2e2179hf1flGwIfocGIJrO",
  "last_id": "asst_AdlLkNpYIKMMwTZyrD3tXnBm",
  "has_more": false
}
```
