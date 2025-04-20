package jp.ukiba.koinu.ko_java
package encoding

class Base32Suite extends munit.FunSuite:
  def roundTrip(base32: Base32)(plain: Array[Byte], encodedExpected: String) =
    val encoded = base32.encode(plain)
    assertEquals(encoded.toSeq, encodedExpected.toSeq)

    val decoded = base32.decode(encoded)
    assertEquals(decoded.toSeq, plain.toSeq)

    val decodedWithoutPadding = base32.decode(encoded.dropRightWhile(_ == '='))
    assertEquals(decodedWithoutPadding.toSeq, plain.toSeq)

  def roundTripAll(plain: Array[Byte], encodedExpected: String) =
    roundTrip(Base32            )(plain, encodedExpected)
    roundTrip(Base32.ExtendedHex)(plain, encodedExpected)
    roundTrip(Base32.Crockford  )(plain, encodedExpected)

  test("Base32"):
    // RFC 4648 Test Vectors
    roundTripAll(Array.empty, "")
    roundTrip(Base32)("f"     .utf8, "MY======")
    roundTrip(Base32)("fo"    .utf8, "MZXQ====")
    roundTrip(Base32)("foo"   .utf8, "MZXW6===")
    roundTrip(Base32)("foob"  .utf8, "MZXW6YQ=")
    roundTrip(Base32)("fooba" .utf8, "MZXW6YTB")
    roundTrip(Base32)("foobar".utf8, "MZXW6YTBOI======")
    roundTrip(Base32.ExtendedHex)("f"     .utf8, "CO======")
    roundTrip(Base32.ExtendedHex)("fo"    .utf8, "CPNG====")
    roundTrip(Base32.ExtendedHex)("foo"   .utf8, "CPNMU===")
    roundTrip(Base32.ExtendedHex)("foob"  .utf8, "CPNMUOG=")
    roundTrip(Base32.ExtendedHex)("fooba" .utf8, "CPNMUOJ1")
    roundTrip(Base32.ExtendedHex)("foobar".utf8, "CPNMUOJ1E8======")

    roundTrip(Base32.Crockford)("foo"   .utf8, "CSQPY===")
    roundTrip(Base32.Crockford)("foobar".utf8, "CSQPYRK1E8======")
    assertEquals(Base32.Crockford.decode("0Oo0").toSeq, Seq[Byte](0, 0))
    assertEquals(Base32.Crockford.decode("1Ii1").toSeq, Seq[Byte](8, 66))
    assertEquals(Base32.Crockford.decode("1Ll1").toSeq, Seq[Byte](8, 66))
