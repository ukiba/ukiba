package jp.ukiba.koneko
package ko_fs2

import fs2.{Stream, Pull, Pipe, RaiseThrowable}
import cats.syntax.all.*

object StreamParser:
  /* A function from a Stream and the parsed result and remaining stream. */
  final case class Parser[F[_]: RaiseThrowable, A, B]( // TODO should A be -A like Kleisli?
    run: Stream[F, A] => Pull[F, Nothing, (B, Stream[F, A])],
  ):
    def map[C](f: B => C): Parser[F, A, C] = Parser: in =>
      run(in).map: (result, rest) =>
        (f(result), rest)

    def flatMap[C](f: B => Parser[F, A, C]): Parser[F, A, C] = Parser: in =>
      run(in).flatMap: (result, rest) =>
        f(result).run(rest)

    def void: Parser[F, A, Unit] = map(_ => ())

    //def orElse[BB >: B](that: Parser[F, A, BB]): Parser[F, A, BB] = ???

    /** Sequences another parser after this one */
    def ~[C](that: Parser[F, A, C]): Parser[F, A, (B, C)] =
      for
        b <- this
        c <- that
      yield (b, c)

    /** Ignore the result of the left-hand parser */
    def *>[C](that: Parser[F, A, C]): Parser[F, A, C] =
      for
        b <- this
        c <- that
      yield c

    /** Ignore the result of the left-hand parser */
    def <*[C](that: Parser[F, A, C]): Parser[F, A, B] =
      for
        b <- this
        c <- that
      yield b

    /** Parses a single result */
    def pipe: Pipe[F, A, B] = in =>
      run(in).flatMap: (result, rest) =>
        Pull.output1(result) >> rest.pull.uncons1.flatMap:
          case Some((hd, tl)) => Pull.raiseError(ParseException(s"No more input is expected: $hd"))
          case None => Pull.done
      .stream

  def pure[F[_]: RaiseThrowable, A, B](result: B): Parser[F, A, B] = Parser: in =>
    Pull.pure((result, in))

  def unit[F[_]: RaiseThrowable, A]: Parser[F, A, Unit] = pure(())

  def failWith[F[_]: RaiseThrowable, A, B](ex: ParseException): Parser[F, A, B] = Parser: _ =>
    Pull.raiseError(ex)

  def failWith[F[_]: RaiseThrowable, A, B](msg: String): Parser[F, A, B] = failWith(ParseException(msg))

  /** Outputs Some input element, or None at the end of the input */
  def anyOption[F[_]: RaiseThrowable, A]: Parser[F, A, Option[A]] = Parser:
    _.pull.uncons1.flatMap:
      case Some((hd, tl)) => Pull.pure((Some(hd), tl))
      case None           => Pull.pure((None    , Stream.empty))

  /** Outputs any input element, or fails at the end of the input */
  def any[F[_]: RaiseThrowable, A]: Parser[F, A, A] = anyOption.flatMap:
    case Some(hd) => pure(hd)
    case None     => failWith(EndOfInput())

  def single[F[_]: RaiseThrowable, A](expected: A): Parser[F, A, Unit] = any.flatMap:
    case `expected` => unit
    case unexpected => failWith(s"Unexpected $unexpected: expected $expected")

  def singleIn[F[_]: RaiseThrowable, A](exps: Iterable[A]): Parser[F, A, A] = anyOption.flatMap:
    case Some(expected) if exps.exists(_ == expected) => pure(expected)
    case Some(unexpected) => failWith(s"Unexpected $unexpected: expected one of ${exps.mkString(", ")}")
    case None             => failWith(EndOfInput())

  def singleIn[F[_]: RaiseThrowable, A](exp0: A, exps: A*): Parser[F, A, A] = singleIn(exp0 +: exps)

  class ParseException(msg: String) extends RuntimeException(msg)
  class EndOfInput[A](expected: Iterable[A] = Iterable.empty) extends ParseException(
    s"More input in expected${Option.when(expected.nonEmpty)(s": ${expected.mkString(", ")}").mkString}"
  )
