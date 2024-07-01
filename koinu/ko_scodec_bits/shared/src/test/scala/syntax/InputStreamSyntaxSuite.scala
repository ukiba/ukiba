package jp.ukiba.koinu
package ko_scodec
package ko_bits
package syntax

import scodec.bits.ByteVector

import java.io.ByteArrayInputStream

class InputStreamSyntaxSuite extends munit.FunSuite:
  test("readAllBytesToByteVector"):
    import syntax.all.*
    assertEquals(ByteArrayInputStream(Array[Byte](0x01)).readAllBytesToByteVector(), ByteVector(0x01))
