package jp.ukiba.koinu.ko_java
package encoding

import scala.math.{min, max}
import scala.annotation.tailrec
import java.nio.{ByteBuffer, CharBuffer}
import java.nio.charset.{Charset, CharsetDecoder, CoderResult, CodingErrorAction}

object CharUtil:
  type DecodePartialResult = (chars: String, numRemainingBytes: Int)

  /**
   * Decodes as many bytes as possible, with given `Charset`.
   * Stops when CoderResult is UNDERFLOW (input exhausted or trailing incomplete sequence),
   * MALFORMED, or UNMAPPABLE.
   * On return, `in.position` is advanced to reflect the bytes consumed.
   *
   * This method does not support incremental decoding.
   * This method is intended for partial/best-effort decoding where trailing bytes may remain undecoded;
   * it never calls `CharsetDecoder.decode` with `endOfInput = true` nor `CharsetDecoder.flush`.
   *
   * This method should work with stateful `CharsetDecoder` like `ISO-2022-JP`,
   * as long as `in` contains the start of the string.
   *
   * When `CharsetDecoder.decode` returns `OVERFLOW` without consuming / producing anything, this stops early.
   * It should not happen unless `CharsetDecoder` has a bug (probably custom `CharsetDecoder`).
   *
   * @param maxBufSize any value less than lower bound `ceil(maxCharsPerByte) * 8`
                       will be treated as the lower bound.
   * @return The decoded characters and the remaining number of bytes after decoding.
   *         This intentionally does not include the reason why the decoding stopped.
   */
  def decodePartial(in: ByteBuffer, charset: Charset, maxBufSize: Int = 1024): DecodePartialResult =
    require(maxBufSize > 0, s"maxBufSize = $maxBufSize")

    if in.remaining == 0 then
      ("", 0)

    else
      val dec = charset.newDecoder()
          .onMalformedInput     (CodingErrorAction.REPORT)
          .onUnmappableCharacter(CodingErrorAction.REPORT)

      decodePartial(in, dec, maxBufSize)

  /**
   * @param dec must be in the initial state, and will not be reset when returning
   */
  protected def decodePartial(in: ByteBuffer, dec: CharsetDecoder, maxBufSize: Int): DecodePartialResult =
      val estimatedOutLen = min((dec.averageCharsPerByte.toDouble * in.remaining).ceil, Int.MaxValue).toInt
      val minBufSize = max(dec.maxCharsPerByte.toDouble.ceil, 1).toInt * 8  // multiply to avoid tight loop
      val bufSize = min(estimatedOutLen, maxBufSize).max(minBufSize)

      val buf = CharBuffer.allocate(bufSize)
      val sb = StringBuilder(bufSize)

      var result = CoderResult.OVERFLOW  // need more output buffer
      while result.isOverflow do
        val pos0 = in.position

        result = dec.decode(in, buf, /*endOfInput =*/ false)

        if result.isOverflow && in.position == pos0 && buf.position == 0 then
          // this should not happen unless CharsetDecoder has a bug; bail out silently
          result = CoderResult.UNDERFLOW

        else if buf.position > 0 then
          buf.flip()
          sb.append(buf: CharSequence)
          buf.clear()

      (sb.toString, in.remaining)

  /**
   * Decodes as many bytes as possible, with given `Charset`.
   * Stops when CoderResult is UNDERFLOW (input exhausted or trailing incomplete sequence),
   * MALFORMED, or UNMAPPABLE.
   *
   * @return The decoded characters and the remaining number of bytes after decoding.
   */
  def decodePartial(bytes: Array[Byte], offset: Int, len: Int, charset: Charset): DecodePartialResult =
    require(offset >= 0 && len >= 0 &&
        offset <= bytes.length && len <= bytes.length - offset,  // overflow safe
      s"offset = $offset, len = $len, bytes.length = ${bytes.length}"
    )

    decodePartial(ByteBuffer.wrap(bytes, offset, len), charset)

  /**
   * Decodes as many bytes as possible, backward from the end, with given `Charset`.
   *
   * This method restores the original position of `in`,
   * but invalidates the mark of `in`.
   *
   * Intended to be used for the format like PDF, where
   * there should be texts at the end but may contain binaries in the middle.
   *
   * @return The decoded characters and the remaining number of bytes before decoding.
   */
  def decodePartialBackward(in: ByteBuffer, charset: Charset, maxBufSize: Int = 1024): DecodePartialResult =
    require(maxBufSize > 0, s"maxBufSize = $maxBufSize")

    if in.remaining == 0 then
      ("", 0)

    else
      val dec = charset.newDecoder()
          .onMalformedInput     (CodingErrorAction.REPORT)
          .onUnmappableCharacter(CodingErrorAction.REPORT)

      val (maxBytesPerChar, maxTries) =
        if CharsetMeta.isIso2022(charset) then  // stateful decoder with escape
          val maxEscapeLen = 4
          val maxBytesPerChar = 2 // should be enough for common ISO-2022-* profiles
          (maxBytesPerChar, maxEscapeLen + maxBytesPerChar + 2)  // add 2, for unusual but legal sequence
        else
          val maxBytesPerChar = CharsetMeta.cached.maxBytesPerCharOf(charset).toDouble.ceil.toInt  // at least 1
          (maxBytesPerChar, maxBytesPerChar)

      val buf2 = CharBuffer.allocate(2)  // accomodates surrogate pair
      val pos0 = in.position

      case class Decoded(chars: String, pos: Int)
      def toResult(decodedOpt: Option[Decoded]): DecodePartialResult = decodedOpt match
        case Some(decoded) => (decoded.chars, decoded.pos - pos0)
        case None          => ("", 0)

      @tailrec def binarySearch(from: Int, until: Int, lastDecoded: Option[Decoded]): DecodePartialResult =
        val middle = ((from.toLong + until) / 2).toInt
        if middle == from || middle + maxBytesPerChar > until then
          toResult(lastDecoded)

        else
          dec.reset()
          firstDecodableOffsetIn(in.position(middle), dec, buf2, maxTries) match
            case Some(offset) =>
              dec.reset()
              buf2.clear()

              // the target range must be re-decoded since
              // UTF-16/32 and JOS-2022 might change the result with earlier escape
              val (chars, numRemainingBytes) = decodePartial(in.position(middle + offset), dec, maxBufSize)
              if numRemainingBytes == 0 then
                binarySearch(from, middle, Some(Decoded(chars, middle + offset)))  // look for longer suffix
              else
                binarySearch(middle, until, lastDecoded)

            case None =>
              // In order to cap the computational cost,
              // this does not try to handle cases for ISO-2022 where
              // 1. escape exists before `middle`, and
              // 2. it would be decodable with that escape
              binarySearch(middle, until, lastDecoded)

      try
        // fast path for when `in` starts at mid-character and the remaining are all text
        firstDecodableOffsetIn(in, dec, buf2, maxTries) match
          case Some(offset) =>
            dec.reset()
            val (chars, numRemainingBytes) = decodePartial(in.position(pos0 + offset), dec, maxBufSize)
            if numRemainingBytes == 0 then
              (chars, offset)
            else
              buf2.clear()
              binarySearch(pos0, in.limit, None)           // slow path

          case None => binarySearch(pos0, in.limit, None)  // slow path

      finally
        in.position(pos0)

  /**
   * Decodes as many bytes as possible, backward from the end, with given `Charset`.
   *
   * Intended to be used for the format like PDF, where
   * there should be texts at the end but may contain binaries in the middle.
   *
   * @return The decoded characters and the remaining number of bytes before decoding.
   */
  def decodePartialBackward(bytes: Array[Byte], offset: Int, len: Int, charset: Charset): DecodePartialResult =
    require(offset >= 0 && len >= 0 &&
        offset <= bytes.length && len <= bytes.length - offset,  // overflow safe
      s"offset = $offset, len = $len, bytes.length = ${bytes.length}"
    )

    decodePartialBackward(ByteBuffer.wrap(bytes, offset, len), charset)

  /**
   * Finds the offset of the first decodable character.
   *
   * This method restores the original position of `in`,
   * but invalidates the mark of `in`.
   *
   * For stateful `CharsetDecoder` like `ISO-2022-JP`,
   * `maxTries` should be increased but inevitably unreliable.
   *
   * @param dec must be in the initial state, and will not be reset when returning
   * @param buf2 must accomodate at least two characters for surrogate pairs
   */
  protected def firstDecodableOffsetIn(in: ByteBuffer, dec: CharsetDecoder,
      buf2: CharBuffer, maxTries: Int): Option[Int] =
    val pos0 = in.position
    try
      var offset = 0
      var found = false
      while offset < maxTries && !found do
        val result = dec.decode(in, buf2, /*endOfInput =*/ false)

        if !result.isError && buf2.position > 0 then
          buf2.flip()
          val ch = buf2.get()
          if ch.isHighSurrogate then
            found = buf2.remaining > 0 && buf2.get().isLowSurrogate
          else if !ch.isLowSurrogate then
            found = true

        if !found then
          offset += 1
          dec.reset()
          in.position(pos0 + offset)
          buf2.clear()

      Option.when(found)(offset)

    finally
      in.position(pos0)

  // TODO there can be much more performant specialization for UTF-8
