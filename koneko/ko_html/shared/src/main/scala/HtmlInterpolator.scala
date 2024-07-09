package jp.ukiba.koneko
package ko_html

import scala.quoted.{Quotes, Expr, Varargs}
import scala.annotation.tailrec

extension (inline sc: StringContext)
  transparent inline def html(inline args: Any*): Any = ${ HtmlInterpolator.impl('sc, 'args) }

object HtmlInterpolator:
  import HtmlAst.{ArgCharMin, MaxArgsLen}

  def impl(scExpr: Expr[StringContext], argsExpr: Expr[Seq[Any]])(using Quotes): Expr[Any] =
    // extract Seq from the expression
    val (parts, args) = (scExpr, argsExpr) match
      case ('{ StringContext(${Varargs(partExprs)}*) }, Varargs(args)) =>
        (partExprs.map(_.valueOrAbort), args.toIndexedSeq)

    // sanity check
    require(parts.length == args.length + 1, s"parts and args mismatch: parts.length = ${
        parts.length}, args.length = ${args.length}")
    require(args.length < MaxArgsLen, s"too many args: ${args.length}")

    val html = interleaveArgs(parts)
    //println(s"html = $html")
    val ast = HtmlParser().parseElem(html)

    //println(s"ast = $ast, args.size = ${args.size}")
    given HtmlCodeGen.GenContext = HtmlCodeGen.GenContext(None)
    val code = HtmlCodeGen(args).createElemTree(ast)
    //println(s"code = ${code.show}")
    code

  def interleaveArgs(parts: Seq[String]): String =
    @tailrec def loop(partsInit: Iterator[(String, Int)], partsLast: String, sb: StringBuilder): String =
      if partsInit.hasNext then
        val (part, index) = partsInit.next
        sb ++= part
        sb += (ArgCharMin + index).toChar
        loop(partsInit, partsLast, sb)

      else
        sb ++= partsLast
        sb.toString

    loop(parts.init.iterator.zipWithIndex, parts.last, StringBuilder())
