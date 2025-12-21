package jp.ukiba.koinu.ko_java

import java.io.InputStream
import java.util.{Arrays => JArrays}

object Hex:
  inline val hexDigits = "0123456789abcdef"
  inline def hexDigitOf(x: Int): Char = hexDigits.charAt(x & 0x0f)
  inline def asciiOf(bt: Byte): Char = if bt >= 0x20 && bt <= 0x7e then bt.toChar else '.'

  extension (sb: StringBuilder)
    inline def appendHex(x: Byte): Unit =
      val unsigned = x & 0xff  // mask with 0xff for negative values

      sb.append(hexDigitOf(unsigned / 16))
      sb.append(hexDigitOf(unsigned % 16))
      /*
        The following code append the same digits as above, and more efficient,
        but the above code is easier to reason, especially for modern programmers,
        and the optimizer should convert the above to the following anyway.

          sb.append(hexDigitOf(unsigned >>> 4))
          sb.append(hexDigitOf(unsigned & 0x0f))
      */

    def appendHexWithPadding(x: Long, minLen: Int, padChar: Char): Unit =
      require(minLen > 0, s"minLen = $minLen")

      val hexLen =
        if x == 0 then 1
        else 16 - java.lang.Long.numberOfLeadingZeros(x) / 4  // numberOfLeadingZeros returns number of bits

      for i <- 0 until minLen - hexLen do  // `Range` allocation can be optimized away by HotSpot
        sb.append(padChar)

      if x == 0 then
        sb.append('0')
      else
        for i <- hexLen - 1 to 0 by -1 do  // `Range` allocation can be optimized away by HotSpot
          val digitIndex = ((x >>> (i * 4)) & 0x0fL).toInt  // >>> is unsigned shift
          sb.append(hexDigits.charAt(digitIndex))

  /*
   * Outputs the same lines as `hexdump -C`, except omitting the final line that contains the total length.
   * BSD / GNU `hexdump -C` are equivalent in this scenario.
   */
  trait DumpIterator extends Iterator[String]:  // should be reused for dump(in: fs2.Chunk[Byte])
    private val dumpLineCapacity =
        8 +        // initial offset width
        2 +
        8 * 3 +    // left hex block
        1 +
        8 * 3 +    // right hex block
        1 +
        1 + 16 + 1 // ascii

    private var offset = 0L  // assumes no overflow
    private var buf     = new Array[Byte](16)
    private var prevBuf = new Array[Byte](16)
    private var sb = StringBuilder(dumpLineCapacity)  // grows only when offset > ffff_ffff
    private var repeated = false

    /** @return filled length, 0 only at EOF */
    def fillBuf(buf: Array[Byte]): Int

    override def hasNext =
      if sb.nonEmpty then
        true
      else
        while true do
          // populate buf
          val len = fillBuf(buf)

          if len == 0 then
            return false  // return is okay here since it is non-local (not within lambda)

          // repeated line
          else if offset > 0 && len == 16 && JArrays.equals(buf, prevBuf) then
            offset += len

            if !repeated then
              sb.append('*')
              repeated = true
              return true

          // unrepeated line
          else
            sb.appendHexWithPadding(offset, 8, '0')  // avoids allocation by f"$offset%08x"
            sb.append("  ")
            offset += len

            System.arraycopy(buf, 0, prevBuf, 0, len)
            repeated = false

            // hex blocks
            for i <- 0 until 16 do
              if i == 8 then
                sb.append(' ')

              if i < len then
                sb.appendHex(buf(i))
                sb.append(' ')
              else
                sb.append("   ")

            // ascii block
            sb.append(' ')
            sb.append('|')
            for i <- 0 until 16 do
              sb.append(if i < len then asciiOf(buf(i)) else ' ')
            sb.append('|')

            return true

        throw AssertionError("unreachable")  // this needs to exist to satisfy the compiler

    override def next() =
      if !hasNext then throw new NoSuchElementException("exhausted")
      val ret = sb.toString
      sb.clear()
      ret

  /** Outputs the same lines as `hexdump -C`, except omitting the final line that contains the total length. */
  def dump(in: Array[Byte], offset: Int, length: Int): Iterator[String] =
    // the out of bound exception from System.arraycopy would not be easy to analyze
    require(offset >= 0, s"offset = $offset")
    require(length >= 0, s"length = $length")
    require(offset.toLong + length <= in.length,
        s"offset ($offset) + length ($length) must not exceed the array length ${in.length}")

    val inLength = length // rename to avoid ambiguity with Iterator.length
    new DumpIterator:
      var index = 0
      override def fillBuf(buf: Array[Byte]) =
        val len = 16.min(inLength - index)
        System.arraycopy(in, offset + index, buf, 0, len)
        index += len
        len

  def dump(in: Array[Byte]): Iterator[String] = dump(in, 0, in.length)

  /** Outputs the same lines as `hexdump -C`, except omitting the final line that contains the total length. */
  def dump(in: InputStream): Iterator[String] =  // this does not close `in`, which is the norm for the library
    new DumpIterator:
      override def fillBuf(buf: Array[Byte]) =
        in.readNBytes(buf, 0, 16)

  /**
   * Outputs the same lines as `hexdump -C`, except omitting the final line that contains the total length.
   * Note: `Iterator[Byte]` usually boxes to java.lang.Byte (generic `next()` erases to `Object` on the JVM).
   */
  def dump(in: Iterator[Byte]): Iterator[String] =
    new DumpIterator:
      override def fillBuf(buf: Array[Byte]) =
        var len = 0
        while len < 16 && in.hasNext do
          buf(len) = in.next
          len += 1
        len

  /**
   * Outputs the same lines as `hexdump -C`, except omitting the final line that contains the total length.
   * Note: `Iterator[Byte]` usually boxes to java.lang.Byte (generic `next()` erases to `Object` on the JVM).
   */
  def dump(bytes: IterableOnce[Byte]): Iterator[String] = dump(bytes.iterator)
