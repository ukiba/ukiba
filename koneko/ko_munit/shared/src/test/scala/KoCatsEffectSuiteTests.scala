package jp.ukiba.koneko
package ko_munit

import munit.TestOptions
import cats.effect.Async

class KoCatsEffectSuiteTests extends KoCatsEffectSuite:
  val succeed = true // change to false to see if Location is correctly passed

  test("t0"):
    Async[F].pure(succeed).assert

  nest("n1"):
    nest("n11"):
      test("t111"):
        Async[F].pure(succeed).assert

    test("t12"):
      Async[F].pure(succeed).assert

    nest("n13"):
      test(TestOptions("t131")):
        Async[F].pure(succeed).assert

  test("t2"):
    Async[F].pure(succeed).assert
