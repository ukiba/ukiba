package jp.ukiba.koinu.ko_java
package encoding

class Base64Suite extends munit.FunSuite:
  def roundTrip(base64: Base64)(plain: Array[Byte], encodedExpected: String) =
    val encoded = base64.encode(plain)
    assertEquals(encoded.toSeq, encodedExpected.toSeq)

    val decoded = base64.decode(encoded)
    assertEquals(decoded.toSeq, plain.toSeq)

    val decodedWithoutPadding = base64.decode(encoded.dropRightWhile(_ == '='))
    assertEquals(decodedWithoutPadding.toSeq, plain.toSeq)

  def roundTripAll(plain: Array[Byte], encodedExpected: String) =
    roundTrip(Base64        )(plain, encodedExpected)
    roundTrip(Base64.UrlSafe)(plain, encodedExpected)

  test("Base64"):
    // RFC 4648 Test Vectors
    roundTripAll(Array.empty, "")
    roundTripAll("f"     .utf8, "Zg==")
    roundTripAll("fo"    .utf8, "Zm8=")
    roundTripAll("foo"   .utf8, "Zm9v")
    roundTripAll("foob"  .utf8, "Zm9vYg==")
    roundTripAll("fooba" .utf8, "Zm9vYmE=")
    roundTripAll("foobar".utf8, "Zm9vYmFy")

    // wikipedia
    roundTripAll("Many hands make light work.".utf8, "TWFueSBoYW5kcyBtYWtlIGxpZ2h0IHdvcmsu")
    roundTripAll("light work.".utf8, "bGlnaHQgd29yay4=")
    roundTripAll("light work" .utf8, "bGlnaHQgd29yaw==")
    roundTripAll("light wor"  .utf8, "bGlnaHQgd29y")
    roundTripAll("light wo"   .utf8, "bGlnaHQgd28=")
    roundTripAll("light w"    .utf8, "bGlnaHQgdw==")
