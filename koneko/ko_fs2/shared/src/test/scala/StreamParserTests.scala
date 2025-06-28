package jp.ukiba.koneko
package ko_fs2

import ko_munit.KoCatsEffectSuite

import fs2.Stream
import cats.syntax.all.*

class StreamParserTests extends KoCatsEffectSuite:
  import StreamParser.{single, singleIn, ParseException}

  nest("Char"):
    nest("unit"):
      test("empty"):
        Stream.emits[F, Char]("")
            .through(StreamParser.unit.pipe)
            .compile.onlyOrError

      test("ParseException: No more input is expected: X"):
        Stream.emits[F, Char]("X")
            .through(StreamParser.unit.pipe)
            .compile.onlyOrError
            .interceptMessage[ParseException]("No more input is expected: X")

    nest("single"):
      test("ParseException: More input in expected"):
        Stream.emits[F, Char]("")
            .through(single('X').pipe)
            .compile.onlyOrError
            .interceptMessage[ParseException]("More input in expected")

      test("success"):
        Stream.emits[F, Char]("X")
            .through(single('X').pipe)
            .compile.onlyOrError

      test("Unexpected Y: expected X"):
        Stream.emits[F, Char]("Y")
            .through(single('X').pipe)
            .compile.onlyOrError
            .interceptMessage[ParseException]("Unexpected Y: expected X")

      test("ParseException: No more input is expected: X"):
        Stream.emits[F, Char]("XX")
            .through(single('X').pipe)
            .compile.onlyOrError
            .interceptMessage[ParseException]("No more input is expected: X")

    nest("singleIn"):
      test("ParseException: More input in expected"):
        Stream.emits[F, Char]("")
            .through(singleIn('X', 'Y').pipe)
            .compile.onlyOrError
            .interceptMessage[ParseException]("More input in expected")

      test("success: X"):
        Stream.emits[F, Char]("X")
            .through(singleIn('X', 'Y').pipe)
            .compile.onlyOrError

      test("success: Y"):
        Stream.emits[F, Char]("Y")
            .through(singleIn('X', 'Y').pipe)
            .compile.onlyOrError

      test("Unexpected Z: expected one of X, Y"):
        Stream.emits[F, Char]("Z")
            .through(singleIn('X', 'Y').pipe)
            .compile.onlyOrError
            .interceptMessage[ParseException]("Unexpected Z: expected one of X, Y")

      test("ParseException: No more input is expected: X"):
        Stream.emits[F, Char]("XX")
            .through(singleIn('X', 'Y').pipe)
            .compile.onlyOrError
            .interceptMessage[ParseException]("No more input is expected: X")

      test("ParseException: No more input is expected: Y"):
        Stream.emits[F, Char]("YY")
            .through(singleIn(Iterable('X', 'Y')).pipe)
            .compile.onlyOrError
            .interceptMessage[ParseException]("No more input is expected: Y")

      test("ParseException: No more input is expected: Z"):
        Stream.emits[F, Char]("XZ")
            .through(singleIn(Iterable('X', 'Y')).pipe)
            .compile.onlyOrError
            .interceptMessage[ParseException]("No more input is expected: Z")

    nest("~"):
      test("X ~ Y"):
        val p: StreamParser.Parser[F, Char, Unit] = (single[F, Char]('X') ~  single('Y')).void
        Stream.emits[F, Char]("XY")
            .through(p.pipe)
            .compile.onlyOrError

      test("X ~ Y ~ Z"):
        val p: StreamParser.Parser[F, Char, Unit] = (single[F, Char]('X') ~  single('Y') ~  single('Z')).void
        Stream.emits[F, Char]("XYZ")
            .through(p.pipe)
            .compile.onlyOrError
