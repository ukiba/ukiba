package jp.ukiba.koneko
package ko_html

import jp.ukiba.koinu.ko_java.ExceptionBase

import cats.parse.{Parser => P, Parser0 => P0}
import cats.parse.Rfc5234.{alpha, digit, hexdig}
import cats.syntax.all.*

class HtmlParser:
  import HtmlParser.*
  import HtmlAst.{Elem, Attr}

  val comment: P[Unit] =
    P.string("<!--") *> (
      P.charsWhile(_ != '-') |
      P.char('-').soft *> (
        P.charWhere(_ != '-') |
        P.char('-').soft *> P.charWhere(_ != '>').void
      )
    ).rep0 *> P.string("-->").void

  val textNode: P[String] = 
    val chars: P[String] = P.charsWhile: ch =>
      ch match
        case ch if `ascii-whitespace`(ch) => false
        case '<'                          => false
        case '&'                          => false // for character reference
        case _ => true

    // leading and trailing whitespaces are trimmed
    // whitespaces between non-whitespaces are replaced with a single space character
    ws0.with1 *> charRefOrOtherRep(chars | comment.as("")).repSep(ws).map(_.reduce(_ ++ " " ++ _))

  // https://html.spec.whatwg.org/multipage/syntax.html#syntax-attributes
  val attr: P[Attr] =
    val name: P[String] = P.charsWhile: ch =>
      ch match
        case ch if control(ch)                  => false
        case ' ' | '"' | '\'' | '>' | '/' | '=' => false
        case ch if noncharacter(ch)             => false
        case _ => true

    val unquotedChars: P[String] = P.charsWhile: ch =>
      ch match
        case ch if `ascii-whitespace`(ch)       => false
        case '"' | '\'' | '=' | '<' | '>' | '`' => false
        case '/'                                => false // for self-closing tag (not in the spec)
        case '&'                                => false // for character reference
        case _ => true
    val eqAndValue: P[String] = ws0.with1 *> P.char('=') ~ ws0 *> (
      charRefOrOtherRep(unquotedChars) |
      charRefOrOtherRep(P.charsWhile(_ != '\'')).?.map(_.mkString).surroundedBy(P.char('\'')) |
      charRefOrOtherRep(P.charsWhile(_ != '"' )).?.map(_.mkString).surroundedBy(P.char('"' ))
    )

    (name ~ eqAndValue.backtrack.?).map: (name, valueOpt) =>
      Attr(name, valueOpt)

  // https://html.spec.whatwg.org/multipage/syntax.html#syntax-tag-name
  //
  // https://html.spec.whatwg.org/multipage/syntax.html#syntax-tag-omission
  //   only <li>, <td>, <th> omissions are supported
  val elem: P[Elem] = P.recursive[Elem]: recurse =>
    val name: P[String] = (alpha ~ (alpha | digit).rep0).string

    val children: P0[Seq[Elem | String]] = (recurse.backtrack | textNode.backtrack).rep0

    (ws0.with1 *> P.char('<') *> name ~
        (ws *> attr.repSep0(ws)).?.map(_.getOrElse(Nil)) ~
        (ws0 *> (
          (P.char('/') *> P.char('>').void).as(true) | // self-closing
          P.char('>').as(false)
        )))
      .flatMap: (nameAndAttrs, selfClosed) =>
        val (name, attrs) = nameAndAttrs

        if selfClosed || voidElemNames.contains(name.toLowerCase) then
          P.pure(Elem(name, attrs))
        else
          val endTag: P[Unit] = P.char('<').void <* P.char('/') <* P.ignoreCase(name).void <* ws0 <* P.char('>')
          (children <* ws0 <* endTag).map: children =>
            Elem(name, attrs, children)

  def parseElem(htmlText: String): Elem = (elem <* ws0).parseAll(htmlText) match
    case Right(parsed) => parsed
    case Left(err) => throw HtmlParseException(s"Failed to parse HTML\n${err.show}")

object HtmlParser:
  // https://html.spec.whatwg.org/multipage/syntax.html#void-elements
  val voidElemNames = Array(
    "area", "base", "br", "col", "embed", "hr", "img", "input", "link", "meta", "source", "track", "wbr",
  )

  // https://infra.spec.whatwg.org/#noncharacter
  inline def noncharacter(ch: Char): Boolean = ch match
    case ch if 0xFDD0 <= ch && ch <= 0xFDEF => true
    case 0xFFFE | 0xFFFF |
         0x1FFFE | 0x1FFFF |
         0x2FFFE | 0x2FFFF |
         0x3FFFE | 0x3FFFF |
         0x4FFFE | 0x4FFFF |
         0x5FFFE | 0x5FFFF |
         0x6FFFE | 0x6FFFF |
         0x7FFFE | 0x7FFFF |
         0x8FFFE | 0x8FFFF |
         0x9FFFE | 0x9FFFF |
         0xAFFFE | 0xAFFFF |
         0xBFFFE | 0xBFFFF |
         0xCFFFE | 0xCFFFF |
         0xDFFFE | 0xDFFFF |
         0xEFFFE | 0xEFFFF |
         0xFFFFE | 0xFFFFF |
         0x10FFFE | 0x10FFFF => true
    case _ => false

  // https://infra.spec.whatwg.org/#ascii-whitespace
  inline def `ascii-whitespace`(ch: Char): Boolean = ch match
    case '\t' | '\n' | '\f' | '\r' | ' ' => true
    case _ => false

  val wsChar: P[Unit] = P.charWhere(`ascii-whitespace`).void
  val ws:  P[Unit]  = wsChar.rep .void
  val ws0: P0[Unit] = wsChar.rep0.void

  // https://infra.spec.whatwg.org/#control
  inline def control(ch: Char): Boolean = ch < 0x20

  val charRef: P[String] = (
    P.char('&') *> (
      (alpha ~ (alpha | digit).rep0).string.map(CharRef.namedOrThrow) |
      P.char('#') *> (
        digit.rep.string.map(CharRef.decimal) |
        (P.char('x') | P.char('X')) *> hexdig.rep.string.map(CharRef.hex)
      )
    ) <* P.char(';')
  )

  def charRefOrOtherRep(other: P[String]): P[String] = (charRef | other).rep.map(_.reduce(_ ++ _))

class HtmlParseException(message: String) extends ExceptionBase(message)
