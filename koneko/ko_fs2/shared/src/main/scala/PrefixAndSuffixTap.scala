package jp.ukiba.koneko
package ko_fs2

import cats.effect.std.Mutex
import cats.effect.{Concurrent, Sync, Ref}
import fs2.{Chunk, Stream}
import cats.syntax.all.*

import scala.reflect.ClassTag
import scala.math.min

object PrefixAndSuffixTap:
  final case class Snapshot[O](prefix: Chunk[O], suffix: Chunk[O], count: Long)

/**
 * Maintains a snapshot of prefix, suffix, and count of the elements that have passed through this tap.
 * The suffix is populated only after the prefix is filled (no overlap).
 */
class PrefixAndSuffixTap[O: ClassTag](prefixLimit: Int, suffixLimit: Int):
  import PrefixAndSuffixTap.Snapshot

  require(prefixLimit >= 0, s"prefixLimit must be non-negative: $prefixLimit")
  require(suffixLimit >= 0, s"suffixLimit must be non-negative: $suffixLimit")

  /*
    Implementation notes:
    1. The generization with ClassTag incurs costs on new Array[O] and Chunk[O].toArray but they are negligible.
  */

  /**
   * This method copies the elements into preallocated arrays.
   * The state is updated as pulled downstream.
   *
   * Note: Do not run the resulting stream more than once,
   * since this method keeps a single state per stream internally.
   */
  def tap[F[_]](in: Stream[F, O])(using F: Sync[F] & Concurrent[F]): F[(Stream[F, O], F[Snapshot[O]])] =
    class State:
      private val prefixBuf: Array[O] = new Array[O](prefixLimit)
      private var prefixLen: Int = 0

      private val suffixRing: Array[O] = new Array[O](suffixLimit)
      private var suffixLen: Int = 0
      private var suffixNextPos: Int = 0

      private var count: Long = 0

      def update(chunk: Chunk[O]): Unit =
        if chunk.size > 0 then
          // append prefix
          val prefixLenDelta = min(prefixLimit - prefixLen, chunk.size)
          if prefixLenDelta > 0 then
            chunk.take(prefixLenDelta).copyToArray(prefixBuf, prefixLen)
            prefixLen += prefixLenDelta

          // update suffix
          if prefixLen == prefixLimit && suffixLimit > 0 then
            val remainingLen = chunk.size - prefixLenDelta
            if remainingLen > 0 then
              val suffixUpdateLen = min(remainingLen, suffixLimit)
              val suffixUpdate = chunk.takeRight(suffixUpdateLen)

              val suffixUpdateRightLen = min(suffixLimit - suffixNextPos, suffixUpdateLen)
              suffixUpdate.take(suffixUpdateRightLen).copyToArray(suffixRing, suffixNextPos)

              val suffixUpdateLeftLen = suffixUpdateLen - suffixUpdateRightLen
              if suffixUpdateLeftLen > 0 then
                suffixUpdate.drop(suffixUpdateRightLen).copyToArray(suffixRing, 0)

              suffixLen = min(suffixLen + suffixUpdateLen, suffixLimit)
              suffixNextPos = (suffixNextPos + suffixUpdateLen) % suffixLimit

          count = count + chunk.size

      def snapshot: Snapshot[O] =
        // copy suffixRing since it will be mutated
        val suffixOut = new Array[O](suffixLen)
        if suffixLen < suffixLimit || suffixNextPos == 0 then // the content is contiguous
          System.arraycopy(suffixRing, 0, suffixOut, 0, suffixLen)
        else
          val nonWrappingLen = suffixLimit - suffixNextPos
          System.arraycopy(suffixRing, suffixNextPos, suffixOut, 0, nonWrappingLen)
          System.arraycopy(suffixRing, 0, suffixOut, nonWrappingLen, suffixNextPos)

        Snapshot(
          Chunk.array(prefixBuf, 0, prefixLen), // update does not mutate the prefixBuf up until prefixLen
          Chunk.array(suffixOut),
          count,
        )

    for
      mutex <- Mutex[F]
    yield
      val state = State()

      def step(chunk: Chunk[O]): F[Unit] =
        mutex.lock.surround: // don't mutate the arrays concurrently
          F.delay:
            state.update(chunk)

      def snap: F[Snapshot[O]] = // may be called concurrently, and intended to be called infrequently
        mutex.lock.surround: // don't read the arrays while mutating
          F.delay:
            state.snapshot

      (in.chunks.evalTap(step).unchunks, snap)
      /*
      The proceeding line is equivalent to the following

      def loop(in: Stream[F, O]): Pull[F, O, Unit] =
        in.pull.uncons.flatMap:
          case Some((head, tail)) => Pull.eval(step(head)) >> Pull.output(head) >> loop(tail)
          case None               => Pull.done

      (loop(in).stream, snap)
      */

  /*
   * This method serves the same purpose as `tap`, internally using `Ref` rather than `Mutex`.
   *
   * Performance consideration:
   * 1. The prefix and suffix keep references to the underlying chunks while within their window.
   *    It is an intentional trade off.
   * 2. This implementation assumes the underlying chunks are immutable,
   *    which might not be true for java.nio integrations.
   * 3. http4s 1.0.0-M46 + fs2-io 3.12.2: it seems the combination emits immutable chunks
   *     1. although the ember client does not copy the buffer
   *        https://github.com/http4s/http4s/blob/v1.0.0-M46/ember-core/shared/src/main/scala/org/http4s/ember/core/Parser.scala#L606-L633
   *        where `read` is constructed at
   *        https://github.com/http4s/http4s/blob/v1.0.0-M46/ember-client/shared/src/main/scala/org/http4s/ember/client/internal/ClientHelpers.scala#L171
   *     1. Socket.read allocates a new array for each chunk.
   *        https://github.com/typelevel/fs2/blob/v3.12.2/io/jvm-native/src/main/scala/fs2/io/net/SocketPlatform.scala#L71-L74
   */
  def tapOnImmutableChunks[F[_]: Concurrent](in: Stream[F, O]): F[(Stream[F, O], F[Snapshot[O]])] =
    for
      ref <- Ref.of[F, Snapshot[O]](Snapshot(Chunk.empty, Chunk.empty, 0))
    yield
      def step(chunk: Chunk[O]): F[Unit] =
        ref.update: prev =>
          // append prefix
          val prefixLenDelta = min(prefixLimit - prev.prefix.size, chunk.size)
          val prefix =
            if prefixLenDelta > 0 then
              val newPrefix = prev.prefix ++ chunk.take(prefixLenDelta)
              if newPrefix.size < prefixLimit then
                newPrefix
              else
                Chunk.array(newPrefix.toArray)  // release the reference to the underlying chunks
            else
              prev.prefix

          // update suffix
          val suffix =
            if prefix.size == prefixLimit && suffixLimit > 0 then
              val remainingLen = chunk.size - prefixLenDelta
              if remainingLen > 0 then
                val suffixUpdateLen = min(remainingLen, suffixLimit)
                prev.suffix.takeRight(suffixLimit - suffixUpdateLen) ++ chunk.takeRight(suffixUpdateLen)
              else
                prev.suffix
            else
              prev.suffix

          Snapshot(prefix, suffix, prev.count + chunk.size)

      (in.chunks.evalTap(step).unchunks, ref.get)

// An example usage
/*
import cats.effect.{IO, IOApp}
object PrefixAndSuffixTapExample extends IOApp.Simple:
  def run =
    val src = Stream.range(0, 10000).map(_.toByte)
    PrefixAndSuffixTap[Byte](100, 100).tap[IO](src).flatMap: (out, snap) =>
      val program =
        out
          .through(identity) // your real pipeline here
          .compile
          .drain

      program.attempt.flatMap:
        case Right(_) => snap.flatMap(snap => IO.println(s"completed count=${snap.count}"))
        case Left(ex) => snap.flatMap(snap => IO.println(s"failed ${ex.getMessage}, count=${snap.count}"))
*/
