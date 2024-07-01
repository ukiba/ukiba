package jp.ukiba.koinu
package ko_scodec
package ko_bits
package syntax

import scodec.bits.ByteVector

import java.io.InputStream

trait InputStreamSyntax:
  extension (in: InputStream)
    def readAllBytesToByteVector(bufSize: Int = KoByteVector.defaultBufSize) =
      KoByteVector.readAllBytes(in, bufSize)
