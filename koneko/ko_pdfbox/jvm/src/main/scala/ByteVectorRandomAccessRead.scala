package jp.ukiba.koneko
package ko_pdfbox

import java.io.IOException
import org.apache.pdfbox.io.{RandomAccessRead, RandomAccessReadView}
import scodec.bits.ByteVector

/*
  pdfbox 3.0.7 `RandomAccessReadBuffer(in)` where `in` is `ByteArrayInputStream` mysteriously slow on AWS ECS.
  For 100MB input, it took
  1. 25 second on AWS ESC (r7g.large)
  2. 1.5 second on EC2 (t4g.micro)
  3. 0.5 second on Mac (Apple silicon)
  Using this class instead of `RandomAccessReadBuffer` took 0.2 second on the same ECS cluster.
*/
final class ByteVectorRandomAccessRead(private val bytes: ByteVector) extends RandomAccessRead:

  private var position: Long = 0L
  private var closed: Boolean = false

  private inline def ensureOpen(): Unit =
    if closed then throw IOException("RandomAccessRead is closed")

  private inline def ensureNonNegative(n: Long, what: String): Unit =
    if n < 0 then throw IOException(s"$what must be >= 0: $n")

  override def close(): Unit =
    closed = true

  override def isClosed(): Boolean =
    closed

  override def length(): Long =
    ensureOpen()
    bytes.size.toLong

  override def getPosition(): Long =
    ensureOpen()
    position

  override def seek(newPosition: Long): Unit =
    ensureOpen()
    ensureNonNegative(newPosition, "position")
    position = newPosition

  override def isEOF(): Boolean =
    ensureOpen()
    position >= bytes.size.toLong

  override def read(): Int =
    ensureOpen()
    if position >= bytes.size.toLong then
      -1
    else
      val b = bytes(position.toInt)
      position += 1
      b & 0xff

  override def read(buffer: Array[Byte], offset: Int, len: Int): Int =
    ensureOpen()

    if buffer == null then
      throw NullPointerException("buffer is null")
    if offset < 0 || len < 0 || offset > buffer.length || offset + len > buffer.length then
      throw IndexOutOfBoundsException(
        s"offset=$offset, len=$len, buffer.length=${buffer.length}"
      )
    if len == 0 then
      0
    else if position >= bytes.size.toLong then
      -1
    else
      val remaining = bytes.size.toLong - position
      val n = math.min(len.toLong, remaining).toInt
      bytes.slice(position, position + n).copyToArray(buffer, offset)
      position += n.toLong
      n

  override def createView(startPosition: Long, streamLength: Long): RandomAccessReadView =
    ensureOpen()
    ensureNonNegative(startPosition, "startPosition")
    ensureNonNegative(streamLength, "streamLength")
    new RandomAccessReadView(this, startPosition, streamLength)

