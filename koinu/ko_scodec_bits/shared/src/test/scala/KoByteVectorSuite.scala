package jp.ukiba.koinu
package ko_scodec
package ko_bits

import scodec.bits.ByteVector

import java.io.ByteArrayInputStream

class KoByteVectorSuite extends munit.FunSuite:
  def readAllBytesFromInputStreamTest(byteVec: ByteVector) =
    // Use ByteVector.toInputStream
    assertEquals(byteVec,
        KoByteVector.readAllBytes(byteVec.toInputStream))

    // Use ByteArrayInputStream
    val byteArr = byteVec.toArray
    assertEquals(byteArr.toSeq,
        KoByteVector.readAllBytes(ByteArrayInputStream(byteArr)).toSeq)

  test("readAllBytesFromInputStream: empty"):
    readAllBytesFromInputStreamTest(ByteVector.empty)

  test("readAllBytesFromInputStream: one byte"):
    readAllBytesFromInputStreamTest(ByteVector(0xff))

  test("readAllBytesFromInputStream: a whole chunk"):
    readAllBytesFromInputStreamTest(ByteVector((0 until KoByteVector.defaultBufSize).map: i =>
      (i % 256).toByte
    ))

  test("readAllBytesFromInputStream: two chunks"):
    readAllBytesFromInputStreamTest(ByteVector((0 until KoByteVector.defaultBufSize + 1).map: i =>
      (i % 256).toByte
    ))
