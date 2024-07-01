package jp.ukiba.koinu
package ko_scodec
package ko_bits

import jp.ukiba.koinu.ko_java.java8compat

import scodec.bits.ByteVector

import scala.annotation.tailrec
import java.io.InputStream

object KoByteVector:
  /**
   * Reads all remaining bytes, blocking until the end of the input is reached.
   * A reverse operation of `ByteVector.toInputStream`
   */
  def readAllBytes(in: InputStream, bufSize: Int = defaultBufSize): ByteVector =
    @tailrec def loop(result: ByteVector): ByteVector =
      val buf = new Array[Byte](bufSize) // allocate a new array for each read, to be wrapped by ByteVector
      in.read(buf) match
        case -1 => result // the end of the input

        case len if len <= 0 => // 0 is returned only when bufSize is 0, those less than -1 is never returned
          throw new IllegalArgumentException(s"InputStream.read(byte[]) returned $len (bufSize = $bufSize)")

        case len =>
          val chunk =
            if len == bufSize then ByteVector.view(buf) // don't copy the array
            else ByteVector(buf, 0, len)                // copy to a smaller array

          loop(result ++ chunk)

    loop(ByteVector.empty)

  val defaultBufSize: Int = java8compat.io.chunkSize
