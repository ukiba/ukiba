package jp.ukiba.koneko
package ko_openai

import cats.syntax.all.*
import io.circe, circe.syntax.*, circe.generic.auto.*, circe.{Encoder, Decoder}

case class ObjectListResponse[A](
  `object`: String, // always "list"
  data: Seq[A],
  first_id: Option[String], // 2024-07-12 null returned from GET v1/vector_stores when no data
  last_id : Option[String], // 2024-07-12 null returned from GET v1/vector_stores when no data
  has_more: Boolean,
)

case class DeleteStatus(
  id: String,
  `object`: String,
  deleted: Boolean,
)

sealed trait tool:
  def `type`: String

// https://platform.openai.com/docs/api-reference/assistants/object
object tool:
  case class `type`(`type`: String) extends tool

  case class file_search(`type`: String, file_search: _file_search.file_search) extends tool
  object file_search:
    def apply(max_num_results: Option[Int] = None, ranking_options: Option[(String, Double)] = None): file_search =
      file_search("file_search", _file_search.file_search(max_num_results,
          ranking_options.map(params => _file_search.file_search.ranking_options(params._1, params._2))))
  object _file_search: // Scala 3.6.3: named differently to avoid Cyclic reference involving val function
    case class file_search(
      max_num_results: Option[Int],
      ranking_options: Option[file_search.ranking_options],
    )
    object file_search:
      case class ranking_options(
        ranker: String,
        score_threshold: Double,
      )

  case class function(`type`: String, function: _function.function) extends tool
  object function:
    def apply(description: String, name: String, strict: Option[Boolean] = None): function =
      function("function", _function.function(description, name, strict))
  object _function: // Scala 3.6.3: named differently to avoid Cyclic reference involving val function
    case class function(
      description: String,
      name: String,
      //parameters
      strict: Option[Boolean],
    )

  given Encoder[tool] = Encoder.instance:
    case x: `type`      => x.asJson
    case x: file_search => x.asJson
    case x: function    => x.asJson

  given Decoder[tool] = List[Decoder[tool]](
      Decoder[function].widen,
      Decoder[file_search].widen,
      Decoder[`type`].widen,
    ).reduceLeft(_ or _)

// https://platform.openai.com/docs/api-reference/assistants/object
case class tool_resources(
  code_interpreter: Option[tool_resources.code_interpreter] = None,
  file_search     : Option[tool_resources.file_search] = None,
)

object tool_resources:
  case class code_interpreter(file_ids: Seq[String])
  case class file_search(
    vector_store_ids: Seq[String] = Nil,
    vector_stores: Seq[file_search.vector_store] = Nil,
  )
  object file_search:
    case class vector_store(
      file_ids: Seq[String],
      //chunking_strategy: Option[?],
      metadata: Map[String, String] = Map.empty,
    )

sealed trait response_format:
  def `type`: String

object response_format:
  case class `type`(`type`: String) extends response_format
  case class json_schema(`type`: String, json_schema: String) extends response_format
  // TODO constructor in companion object like image_file

  given Encoder[response_format] = Encoder.instance:
    case x: `type`      => x.asJson
    case x: json_schema => x.asJson

  given Decoder[response_format] = List[Decoder[response_format]](
      Decoder[json_schema].widen,
      Decoder[`type`].widen,
    ).reduceLeft(_ or _)

  given Encoder[String | response_format] = Encoder.instance:
    case x: String          => x.asJson
    case x: response_format => x.asJson

  given Decoder[String | response_format] = List[Decoder[String | response_format]](
      Decoder[response_format].widen,
      Decoder[String].widen,
    ).reduceLeft(_ or _)
