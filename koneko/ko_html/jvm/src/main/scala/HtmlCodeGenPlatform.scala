package jp.ukiba.koneko
package ko_html

import HtmlAst.{Elem, Attr, ArgCharMin, ArgCharMax}
import HtmlCodeGen.GenContext

import scala.quoted.{Quotes, Expr}

abstract private[ko_html] class HtmlCodeGenPlatform(args: Seq[Expr[Any]])(using Quotes):
  def createElemTree(ast: Elem)(using ctx: GenContext): Expr[Any] = ???
