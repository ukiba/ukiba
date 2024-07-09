package jp.ukiba.koneko
package ko_munit

import munit.TestOptions

class KoTestSuiteTests extends KoTestSuite:
  val succeed = true // change to false to see if Location is correctly passed

  test("t0"):
    assert(succeed)

  nest("n1"):
    nest("n11"):
      test("t111"):
        assert(succeed)

    test("t12"):
      assert(succeed)

    nest("n13"):
      test(TestOptions("t131")):
        assert(succeed)

  test("t2"):
    assert(succeed)
