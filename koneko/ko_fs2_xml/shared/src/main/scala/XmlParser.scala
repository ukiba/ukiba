package jp.ukiba.koneko
package ko_fs2
package xml

import fs2.data.xml.{XmlEvent, QName}, XmlEvent.{StartTag, EndTag, XmlString, StartDocument, XmlDecl, EndDocument}
import fs2.{Stream, Pull, Pipe, RaiseThrowable}
import cats.syntax.all.*

/** A simple DSL to parse XmlEvent */
object XmlParser:
  /*
    although fs2.data.xml.dom.DocumentBuilder can produce a case class,
    the child node is visited before the parent node, thus the context is not available
  */

  /*
    https://github.com/dylemma/xml-spac has parsers that integrate with fs2-data, but the last commit is 2023-10.
  */

  /* A function from a Stream and the parsed result and remaining stream. */
  final case class Parser[F[_]: RaiseThrowable, A](
    run: Stream[F, XmlEvent] => Pull[F, Nothing, (A, Stream[F, XmlEvent])],
  ):
    def pipe: Pipe[F, XmlEvent, A] = st =>
      run(st).flatMap: (a, rest) =>
        Pull.output1(a) >> rest.pull.uncons1.flatMap:
          case Some((hd, tl)) => Pull.raiseError(ParseException(s"Unexpected $hd: no more XML nodes are expected"))
          case None => Pull.done
      .stream

    def map[B](f: A => B): Parser[F, B] = Parser: st =>
      run(st).map: (a, rest) =>
        (f(a), rest)

    def flatMap[B](f: A => Parser[F, B]): Parser[F, B] = Parser: st =>
      run(st).flatMap: (a, rest) =>
        f(a).run(rest)

  def pure[F[_]: RaiseThrowable, A](a: A): Parser[F, A] = Parser: st =>
    Pull.pure((a, st))

  def unit[F[_]: RaiseThrowable]: Parser[F, Unit] = pure(())

  def fail[F[_]: RaiseThrowable, A](msg: String): Parser[F, A] = Parser: _ =>
    Pull.raiseError(ParseException(msg))

  def next[F[_]: RaiseThrowable]: Parser[F, XmlEvent] = Parser:
    _.pull.uncons1.flatMap:
      case Some((hd, tl)) => Pull.pure((hd, tl))
      case None           => Pull.raiseError(ParseException(s"More XML nodes are expected"))

  def expect[F[_]: RaiseThrowable](ev: XmlEvent): Parser[F, Unit] = next.flatMap:
    case `ev`  => unit
    case other => fail(s"Unexpected $other: expected $ev")

  /** Apply the given partial function */
  def optPat[F[_]: RaiseThrowable, A](pf: PartialFunction[XmlEvent, Parser[F, A]]): Parser[F, Option[A]] = Parser:
    _.pull.uncons1.flatMap:
      case Some((hd, tl)) =>
        // For all partial function literals the compiler generates an `applyOrElse` implementation which
        // avoids double evaluation of pattern matchers and guards.
        // https://github.com/scala/scala/blob/2.13.x/src/library/scala/PartialFunction.scala#L205
        hd match // no double evaluation like `applyOrElse`
          case pf(parser) =>
            parser.run(tl.cons1(hd)).flatMap: (a, rest) =>
              Pull.pure(Some(a), rest)

          case _ => Pull.pure(None, tl.cons1(hd)) // end of matches

      case None => Pull.pure(None, Stream.empty) // end of stream

  /** Apply the given partial function repeatedly, until the partial functions does not match */
  def repPat[F[_]: RaiseThrowable, A](pf: PartialFunction[XmlEvent, Parser[F, A]]): Parser[F, Seq[A]] =
    // TODO can optPat be reused?
    def loop(results: Seq[A]): Parser[F, Seq[A]] = Parser:
      _.pull.uncons1.flatMap:
        case Some((hd, tl)) =>
          hd match
            case pf(parser) =>
              parser.run(tl.cons1(hd)).flatMap: (a, rest) =>
                loop(results :+ a).run(rest)

            case _ => Pull.pure(results, tl.cons1(hd)) // end of matches

        case None => Pull.pure(results, Stream.empty) // end of stream

    loop(Nil)

  def startTag[F[_]: RaiseThrowable](local: String): Parser[F, Unit] = next.flatMap:
    case StartTag(QName(_, `local`), _, _) => unit
    case other                             => fail(s"Unexpected $other: expected <$local>")

  def endTag[F[_]: RaiseThrowable](local: String): Parser[F, Unit] = next.flatMap:
    case EndTag(QName(_, `local`)) => unit
    case other                     => fail(s"Unexpected $other: expected </$local>")

  def text[F[_]: RaiseThrowable]: Parser[F, String] = next.flatMap:
    case XmlString(str, _) => pure(str.trim)
    case other             => fail(s"Unexpected $other: expected text")

  def textOnlyTag[F[_]: RaiseThrowable](name: String): Parser[F, String] =
    for
      _   <- startTag(name)
      str <- text
      _   <- endTag(name)
    yield str

  def textOnlyTagOpt[F[_]: RaiseThrowable](name: String): Parser[F, Option[String]] =
    optPat:
      case head @ StartTag(QName(_, name), _, _) =>
        for
          _            <- next // should be the same as head
          str <- text
          _   <- endTag(name)
        yield str

  def doc[F[_]: RaiseThrowable, A](p: Parser[F, A]): Parser[F, A] =
    for
      _   <- expect(StartDocument)
      _   <- next.flatMap:
        case XmlDecl(_, _, _) => unit
        case other            => fail(s"Unexpected $other: expected XmlDecl")
      ret <- p
      _   <- expect(EndDocument)
    yield ret

  final class ParseException(msg: String) extends RuntimeException(msg)
