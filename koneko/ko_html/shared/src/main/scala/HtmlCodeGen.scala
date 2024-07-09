package jp.ukiba.koneko
package ko_html

import scala.quoted.{Quotes, Expr}

class HtmlCodeGen(args: Seq[Expr[Any]])(using Quotes) extends HtmlCodeGenPlatform(args)

object HtmlCodeGen:
  case class GenContext(
    namespaceURI: Option[Expr[String]],
  )
