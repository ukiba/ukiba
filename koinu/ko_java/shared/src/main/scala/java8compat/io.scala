package jp.ukiba.koinu.ko_java.java8compat

import scala.math.{min, addExact}
import scala.annotation.{tailrec, targetName}
import java.io.*

/** Provides Java 8 compatibilities for java.io classes */
package object io:
  val chunkSize = 16 * 1024

  // InputStream
  opaque type InputStreamJava8Compat = InputStream
  extension (in: InputStream)
    def java8compat: InputStreamJava8Compat = in
  extension (in: InputStreamJava8Compat)
    // since Java 9
    def readAllBytes(): Array[Byte] = readNBytes(Int.MaxValue)

    @targetName("readAllBytesJavaCompat")
    def readAllBytes: Array[Byte] = readAllBytes() // let the parameters be omitted like Java API

    def readNBytes(buf: Array[Byte], off: Int, len: Int): Int =
      if off < 0 then throw IndexOutOfBoundsException(s"off = $off")
      if len < 0 then throw IndexOutOfBoundsException(s"len = $len")
      if len > buf.length - off then
        throw IndexOutOfBoundsException(s"len = $len while off = $off / ${buf.length}")

      @tailrec def loop(i: Int): Int =
        if i < len then
          val n = in.read(buf, off + i, len - i)
          if n >= 0 then
            loop(i + n)
          else
            i // EOF
        else
          i // reached the requested length

      loop(0)

    /*
      [Android](https://developer.android.com/reference/java/io/InputStream#transferTo(java.io.OutputStream))
      1. Available by [desugaring](https://developer.android.com/studio/write/java11-nio-support-table)
    */
    def transferTo(out: OutputStream): Long =
      if out == null then throw NullPointerException("out is null")

      val chunk = new Array[Byte](chunkSize)
      @tailrec def loop(count: Long): Long =
        in.read(chunk, 0, chunk.length) match
          case n if n < 0 => count // EOF
          case n =>
            if n > 0 then out.write(chunk, 0, n)
            val sum = try
              addExact(count, n)
            catch
              case ex: ArithmeticException => Long.MaxValue
            loop(sum)

      loop(0)

    // since Java 11
    def readNBytes(len: Int): Array[Byte] =
      if len < 0 then throw IllegalArgumentException(s"len = $len")

      // read in chunks
      @tailrec def loop(chunks: List[Array[Byte]], off: Int): (List[Array[Byte]], Int) =
        if off < len then
          val chunk = new Array[Byte](min(chunkSize, len - off))
          val n = readNBytes(chunk, 0, chunk.length)
          if n == chunk.size then
            loop(chunks :+ chunk, off + n) // read next chunk
          else
            (chunks :+ chunk, off + n) // EOF

        else
          (chunks, off) // reached the requested length

      // concatenate into a single array
      val (chunks, chunksTotal) = loop(Nil, 0)
      val result = new Array[Byte](chunksTotal)
      chunks.foldLeft(0): (off, chunk) =>
        System.arraycopy(chunk, 0, result, off, min(chunk.length, chunksTotal - off))
        off + chunk.length
      result

    // since Java 12
    def skipNByte(len: Long): Unit =
      @tailrec def loop(i: Long): Unit =
        if i < len then
          val n = in.skip(len - i)
          if n > 0 then
            loop(i + n)
          else if n == 0 then
            if in.read() < 0 then
              throw EOFException()
            else
              loop(i - 1)

      loop(0)
