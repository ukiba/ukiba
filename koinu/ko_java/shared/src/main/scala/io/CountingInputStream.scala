package jp.ukiba.koinu.ko_java
package io

import scala.annotation.tailrec
import java.io.{InputStream, OutputStream, EOFException, IOException}

/*
  Enhances InputStream to keep the count (number of bytes read) and if it has reached EOF.

  This class does not extend FilterInputStream,
  because the count needs to be updated in all the read/skip methods anyway,
  to handle all the subclasses of InputStream.

  Overriding select methods that would be eventually called in the default implementation fails in subclasses.
  Also it would be complex: for example skip(Int) calls read()
  https://github.com/openjdk/jdk21u/blob/master/src/java.base/share/classes/java/io/InputStream.java#L287
*/
class CountingInputStream(in: InputStream) extends InputStream:
  private var _count: Long = 0
  def count = _count
  def intCount: Either[String, Int] =
    if Int.MinValue <= _count && _count <= Int.MaxValue then
      Right(_count.intValue)
    else
      Left(s"$_count is not in the range of Int")

  private var _eof: Boolean = false
  def eof = _eof

  private var _marked: Option[Long] = None

  private inline def _read(): Int =
    val prevCount = _count
    val byteResult = in.read()
    if byteResult == -1 then
      if !_eof then
        _eof = true
    else
      if _count == prevCount then // another method might have already updated the count
        _count += 1
    byteResult

  @throws[IOException]
  override def read(): Int = _read()

  private inline def updateCountWithLenResult(isEOF: Int => Boolean)(fn: => Int): Int =
    val prevCount = _count
    val lenResult = fn
    if isEOF(lenResult) then
      if !_eof then
        _eof = true
    else
      if _count == prevCount then // another method might have already updated the count
        _count += lenResult
    lenResult

  @throws[IOException]
  override def read(buf: Array[Byte]): Int =
    updateCountWithLenResult(isEOF = _ == -1):
      in.read(buf)

  @throws[IOException]
  override def read(buf: Array[Byte], off: Int, len: Int): Int =
    updateCountWithLenResult(isEOF = _ == -1):
      in.read(buf, off, len)

  @throws[IOException]
  override def readAllBytes(): Array[Byte] =
    val prevCount = _count
    val arrayResult = in.readAllBytes()
    _eof = true
    if arrayResult.length > 0 && _count == prevCount then // another method might have already updated the count
      _count += arrayResult.length
    arrayResult

  @throws[IOException]
  override def readNBytes(len: Int): Array[Byte] =
    val prevCount = _count
    val arrayResult = in.readNBytes(len)
    if arrayResult.length == 0 then
      if !_eof then
        _eof = true
    else
      if _count == prevCount then // another method might have already updated the count
        _count += arrayResult.length
    arrayResult

  @throws[IOException]
  override def readNBytes(buf: Array[Byte], off: Int, len: Int): Int =
    updateCountWithLenResult(isEOF = _ == 0):
      in.readNBytes(buf, off, len)

  private inline def _skip(len: Long): Long =
    val prevCount = _count
    val lenResult = in.skip(len) // might return 0 even if not EOF
    if lenResult > 0 && _count == prevCount then // another method might have already updated the count
      _count += lenResult
    lenResult

  @throws[IOException]
  override def skip(len: Long): Long = _skip(len)

  @throws[IOException]
  override def skipNBytes(len: Long): Unit =
    @tailrec def loop(len: Long): Unit =
      if len > 0 then
        // similar implementation as
        // https://github.com/openjdk/jdk21u/blob/master/src/java.base/share/classes/java/io/InputStream.java#L604

        val lenResult = _skip(len) // this updates the count
        if lenResult > len then
          throw IOException(s"skipped too much: $lenResult > $len")

        else if lenResult < 0 then
          throw IOException(s"skipped negative: $lenResult")

        else if lenResult == 0 then
          if _read() == -1 then // this updates the count
            throw EOFException()
          loop(len - 1)

        else
          loop(len - lenResult)
    loop(len)

  @throws[IOException]
  override def available(): Int =
    in.available()

  @throws[IOException]
  override def close(): Unit =
    in.close()

  override def mark(readlimit: Int): Unit = // does not throw IOException
    in.mark(readlimit)
    _marked = Some(_count)

  @throws[IOException]
  override def reset(): Unit =
    in.reset()
    _count = _marked.getOrElse(throw IOException("not marked"))

  override def markSupported(): Boolean =
    in.markSupported()

  @throws[IOException]
  override def transferTo(out: OutputStream): Long =
    val prevCount = _count
    val result = in.transferTo(out)
    _eof = true
    if _count == prevCount then // another method might have already updated the count
      _count += result
    result
