package jp.ukiba.koneko
package ko_fs2

import ko_munit.KoCatsEffectSuite

import fs2.{Stream, Chunk, Pull}
import cats.syntax.all.*
import cats.effect.Deferred

import scala.reflect.ClassTag
import scala.math.min

class PrefixAndSuffixTapTests extends KoCatsEffectSuite:
  import PrefixAndSuffixTap.Snapshot

  private def streamOf(bytes: Array[Byte]): Stream[F, Byte] =
    Stream.chunk(Chunk.array(bytes))

  private def streamOfChunks(chunks: Seq[Array[Byte]]): Stream[F, Byte] =
    Stream.emits(chunks).flatMap(streamOf)

  private def waitUntilTotal[O](get: F[Snapshot[O]], expectedAtLeast: Long): F[Snapshot[O]] =
    def loop: F[Snapshot[O]] =
      get.flatMap: s =>
        if s.count >= expectedAtLeast then F.pure(s)
        else F.cede >> loop // explicit boundary because `get` may contain `Ref.get` (hot spin)
    loop

  private def expectedSuffixNoOverlap[O: ClassTag](all: Array[O], prefixLimit: Int, suffixLimit: Int): Array[O] =
    if suffixLimit <= 0 then
      Array.empty[O]
    else if prefixLimit <= 0 then
      all.takeRight(suffixLimit)
    else
      val post = all.drop(min(prefixLimit, all.length))
      post.takeRight(min(suffixLimit, post.length))

  private def assertSnapshot[O: ClassTag](
      snap: Snapshot[O],
      expectedPrefix: Array[O],
      expectedSuffix: Array[O],
      expectedTotal: Long,
  ): Unit =
    // .toArray.toSeq has fewer allocations than .toList
    assertEquals(snap.count, expectedTotal)
    assertEquals(snap.prefix.toArray.toSeq, expectedPrefix.toSeq)
    assertEquals(snap.suffix.toArray.toSeq, expectedSuffix.toSeq)

  nest("constructor requires non-negative limits"):
    test("prefixLimit < 0 throws"):
      interceptMessage[IllegalArgumentException]("requirement failed: prefixLimit must be non-negative: -1"):
        PrefixAndSuffixTap[Byte](-1, 0)

    test("suffixLimit < 0 throws"):
      interceptMessage[IllegalArgumentException]("requirement failed: suffixLimit must be non-negative: -1"):
        PrefixAndSuffixTap[Byte](0, -1)

  nest("empty stream -> empty prefix/suffix, count=0"):
    val in = Stream.empty.covary[F]
    val tap = PrefixAndSuffixTap[Byte](prefixLimit = 10, suffixLimit = 5)

    def withTap(f: Stream[F, Byte] => F[(Stream[F, Byte], F[Snapshot[Byte]])]) =
      f(in).flatMap: (out, snap) =>
        out.compile.drain >> snap.map: snap =>
          assertSnapshot(
            snap,
            expectedPrefix = Array.emptyByteArray,
            expectedSuffix = Array.emptyByteArray,
            expectedTotal = 0,
          )

    test("tap"):
      withTap(tap.tap)
    test("tapOnImmutableChunks"):
      withTap(tap.tapOnImmutableChunks)

  nest("output is identical to input (multi-chunk), snap matches at end"):
    val inBytes = (0 until 32).map(_.toByte).toArray
    val in =
      streamOfChunks(
        List(
          inBytes.slice( 0,  3),
          inBytes.slice( 3, 10),
          inBytes.slice(10, 11),
          inBytes.slice(11, 32),
        )
      )
    val tap = PrefixAndSuffixTap[Byte](prefixLimit = 10, suffixLimit = 5)

    def withTap(f: Stream[F, Byte] => F[(Stream[F, Byte], F[Snapshot[Byte]])]) =
      f(in).flatMap: (out, snap) =>
        out.compile.to(Array).flatMap: outArr =>
          snap.flatMap: snap =>
            F.delay:
              assertEquals(outArr.toSeq, inBytes.toSeq)
              assertSnapshot(
                snap,
                expectedPrefix = inBytes.take(10),
                expectedSuffix = expectedSuffixNoOverlap(inBytes, prefixLimit = 10, suffixLimit = 5),
                expectedTotal = 32,
              )

    test("tap"):
      withTap(tap.tap)
    test("tapOnImmutableChunks"):
      withTap(tap.tapOnImmutableChunks)

  nest("empty chunks do not affect prefix/suffix/count"):
    val c1 = Array[Byte](1, 2, 3)
    val c2 = Array[Byte](4, 5)

    val in = streamOfChunks(Seq(c1, Array.emptyByteArray, c2))
    val tap = PrefixAndSuffixTap[Byte](prefixLimit = 10, suffixLimit = 3)

    def withTap(f: Stream[F, Byte] => F[(Stream[F, Byte], F[Snapshot[Byte]])]) =
      f(in).flatMap: (out, snap) =>
        out.compile.drain >> snap.map: snap =>
          val all = c1 ++ c2
          assertSnapshot(
            snap,
            expectedPrefix = all,
            expectedSuffix = expectedSuffixNoOverlap(all, prefixLimit = 10, suffixLimit = 3), // now empty
            expectedTotal = all.length.toLong
          )

    test("tap"):
      withTap(tap.tap)
    test("tapOnImmutableChunks"):
      withTap(tap.tapOnImmutableChunks)

  nest("short stream shorter than prefixLimit and suffixLimit"):
    val inBytes = Array[Byte](1, 2, 3)
    val in = streamOf(inBytes)
    val tap = PrefixAndSuffixTap[Byte](prefixLimit = 10, suffixLimit = 10)

    def withTap(f: Stream[F, Byte] => F[(Stream[F, Byte], F[Snapshot[Byte]])]) =
      f(in).flatMap: (out, snap) =>
        out.compile.to(Array).flatMap: outArr =>
          snap.map: snap =>
            assertEquals(outArr.toSeq, inBytes.toSeq)
            assertSnapshot(
              snap,
              expectedPrefix = inBytes,
              expectedSuffix = expectedSuffixNoOverlap(inBytes, prefixLimit = 10, suffixLimit = 10), // now empty
              expectedTotal = 3,
            )

    test("tap"):
      withTap(tap.tap)
    test("tapOnImmutableChunks"):
      withTap(tap.tapOnImmutableChunks)

  nest("prefix fills exactly at chunk boundary; suffix starts on next chunk"):
    val c1 = Array[Byte](0, 1, 2, 3, 4)  // exactly fills prefixLimit=5
    val c2 = Array[Byte](5, 6, 7, 8)
    val in = streamOfChunks(List(c1, c2))
    val tap = PrefixAndSuffixTap[Byte](prefixLimit = 5, suffixLimit = 3)

    def withTap(f: Stream[F, Byte] => F[(Stream[F, Byte], F[Snapshot[Byte]])]) =
      f(in).flatMap: (out, snap) =>
        out.compile.drain >> snap.map: snap =>
          val all = c1 ++ c2
          assertSnapshot(
            snap,
            expectedPrefix = all.take(5),
            expectedSuffix = expectedSuffixNoOverlap(all, prefixLimit = 5, suffixLimit = 3),
            expectedTotal  = all.length.toLong,
          )

    test("tap"):
      withTap(tap.tap)
    test("tapOnImmutableChunks"):
      withTap(tap.tapOnImmutableChunks)

  nest("prefix fills across chunk boundary"):
    val c1 = Array[Byte](0, 1)
    val c2 = Array[Byte](2, 3, 4, 5, 6, 7)
    val in = streamOfChunks(List(c1, c2))
    val tap = PrefixAndSuffixTap[Byte](prefixLimit = 5, suffixLimit = 3)

    def withTap(f: Stream[F, Byte] => F[(Stream[F, Byte], F[Snapshot[Byte]])]) =
      f(in).flatMap: (out, snap) =>
        out.compile.drain >> snap.map: snap =>
          val all = c1 ++ c2
          assertSnapshot(
            snap,
            expectedPrefix = all.take(5),
            expectedSuffix = expectedSuffixNoOverlap(all, prefixLimit = 5, suffixLimit = 3),
            expectedTotal = all.length.toLong,
          )

    test("tap"):
      withTap(tap.tap)
    test("tapOnImmutableChunks"):
      withTap(tap.tapOnImmutableChunks)

  nest("suffix ring wraps correctly across multiple chunks"):
    // suffixLimit = 5, bytes 0..9 => suffix should be 5..9 (still true since prefixLimit=3)
    val inBytes = (0 until 10).map(_.toByte).toArray
    val in =
      streamOfChunks(
        List(
          inBytes.slice(0,  2), // 0,1
          inBytes.slice(2,  7), // 2..6
          inBytes.slice(7, 10), // 7..9
        )
      )
    val tap = PrefixAndSuffixTap[Byte](prefixLimit = 3, suffixLimit = 5)

    def withTap(f: Stream[F, Byte] => F[(Stream[F, Byte], F[Snapshot[Byte]])]) =
      f(in).flatMap: (out, snap) =>
        out.compile.drain >> snap.map: snap =>
          assertSnapshot(
            snap,
            expectedPrefix = inBytes.take(3), // 0,1,2
            expectedSuffix = expectedSuffixNoOverlap(inBytes, prefixLimit = 3, suffixLimit = 5),
            expectedTotal = 10,
          )

    test("tap"):
      withTap(tap.tap)
    test("tapOnImmutableChunks"):
      withTap(tap.tapOnImmutableChunks)

  nest("suffix update wraps within a single chunk update (right+left copy)"):
    val c1 = Array[Byte](0, 1, 2, 3) // suffixNextPos becomes 4 when suffixLimit=5
    val c2 = Array[Byte](4, 5, 6)    // updateLen=3 wraps (1 right, 2 left)
    val in = streamOfChunks(List(c1, c2))
    val tap = PrefixAndSuffixTap[Byte](prefixLimit = 0, suffixLimit = 5)

    def withTap(f: Stream[F, Byte] => F[(Stream[F, Byte], F[Snapshot[Byte]])]) =
      f(in).flatMap: (out, snap) =>
        out.compile.drain >> snap.map: snap =>
          val all = c1 ++ c2
          assertSnapshot(
            snap,
            expectedPrefix = Array.emptyByteArray,
            expectedSuffix = expectedSuffixNoOverlap(all, prefixLimit = 0, suffixLimit = 5),
            expectedTotal = all.length.toLong,
          )

    test("tap"):
      withTap(tap.tap)
    test("tapOnImmutableChunks"):
      withTap(tap.tapOnImmutableChunks)

  nest("single big chunk larger than suffixLimit keeps only the last suffixLimit bytes"):
    val inBytes = (0 until 20).map(_.toByte).toArray
    val in = streamOf(inBytes)
    val tap = PrefixAndSuffixTap[Byte](prefixLimit = 7, suffixLimit = 5)

    def withTap(f: Stream[F, Byte] => F[(Stream[F, Byte], F[Snapshot[Byte]])]) =
      f(in).flatMap: (out, snap) =>
        out.compile.drain >> snap.map: snap =>
          assertSnapshot(
            snap,
            expectedPrefix = inBytes.take(7),
            expectedSuffix = expectedSuffixNoOverlap(inBytes, prefixLimit = 7, suffixLimit = 5),
            expectedTotal = 20,
          )

    test("tap"):
      withTap(tap.tap)
    test("tapOnImmutableChunks"):
      withTap(tap.tapOnImmutableChunks)

  nest("prefixLimit=0 and suffixLimit=0 -> always empty prefix/suffix, count increments"):
    val inBytes = (0 until 12).map(_.toByte).toArray
    val in = streamOfChunks(List(inBytes.slice(0, 5), inBytes.slice(5, 12)))
    val tap = PrefixAndSuffixTap[Byte](prefixLimit = 0, suffixLimit = 0)

    def withTap(f: Stream[F, Byte] => F[(Stream[F, Byte], F[Snapshot[Byte]])]) =
      f(in).flatMap: (out, snap) =>
        out.compile.drain >> snap.map: snap =>
          assertSnapshot(
            snap,
            expectedPrefix = Array.emptyByteArray,
            expectedSuffix = Array.emptyByteArray,
            expectedTotal = 12,
          )

    test("tap"):
      withTap(tap.tap)
    test("tapOnImmutableChunks"):
      withTap(tap.tapOnImmutableChunks)

  nest("suffixLimit=0 -> suffix always empty, count is tracked"):
    val inBytes = (0 until 21).map(_.toByte).toArray
    val in = streamOfChunks(List(inBytes.slice(0, 7), inBytes.slice(7, 21)))
    val tap = PrefixAndSuffixTap[Byte](prefixLimit = 5, suffixLimit = 0)

    def withTap(f: Stream[F, Byte] => F[(Stream[F, Byte], F[Snapshot[Byte]])]) =
      f(in).flatMap: (out, snap) =>
        out.compile.drain >> snap.map: snap =>
          assertSnapshot(
            snap,
            expectedPrefix = inBytes.take(5),
            expectedSuffix = Array.emptyByteArray,
            expectedTotal = 21,
          )

    test("tap"):
      withTap(tap.tap)
    test("tapOnImmutableChunks"):
      withTap(tap.tapOnImmutableChunks)

  nest("prefixLimit=0 -> prefix always empty, suffix works"):
    val inBytes = (0 until 21).map(_.toByte).toArray
    val in = streamOfChunks(List(inBytes.slice(0, 7), inBytes.slice(7, 21)))
    val tap = PrefixAndSuffixTap[Byte](prefixLimit = 0, suffixLimit = 5)

    def withTap(f: Stream[F, Byte] => F[(Stream[F, Byte], F[Snapshot[Byte]])]) =
      f(in).flatMap: (out, snap) =>
        out.compile.drain >> snap.map: snap =>
          assertSnapshot(
            snap,
            expectedPrefix = Array.emptyByteArray,
            expectedSuffix = expectedSuffixNoOverlap(inBytes, prefixLimit = 0, suffixLimit = 5),
            expectedTotal = 21,
          )

    test("tap"):
      withTap(tap.tap)
    test("tapOnImmutableChunks"):
      withTap(tap.tapOnImmutableChunks)

  nest("source failure: output bytes before failure are identical"):
    val good1 = Array[Byte](10, 11, 12, 13)
    val good2 = Array[Byte](20, 21)
    val in = streamOf(good1) ++ streamOf(good2) ++ Stream.raiseError[F](new RuntimeException("boom"))
    val tap = PrefixAndSuffixTap[Byte](prefixLimit = 10, suffixLimit = 3)

    def withTap(f: Stream[F, Byte] => F[(Stream[F, Byte], F[Snapshot[Byte]])]) =
      f(in).flatMap: (out, snap) =>
        val recoveredOut = out.handleErrorWith(_ => Stream.empty)
        recoveredOut.compile.to(Array).flatMap: outArr =>
          snap.map: snap =>
            val seen = good1 ++ good2
            assertEquals(outArr.toSeq, seen.toSeq)
            assertSnapshot(
              snap,
              expectedPrefix = seen,
              expectedSuffix = expectedSuffixNoOverlap(seen, prefixLimit = 10, suffixLimit = 3), // now empty
              expectedTotal = seen.length.toLong,
            )

    test("tap"):
      withTap(tap.tap)
    test("tapOnImmutableChunks"):
      withTap(tap.tapOnImmutableChunks)

  nest("source failure after prefix is full: suffix matches post-prefix tail, count is preserved"):
    val prefixLimit = 3
    val suffixLimit = 4

    val c1 = Array[Byte](0, 1, 2)                 // fills prefix exactly
    val c2 = Array[Byte](3, 4, 5, 6, 7, 8)        // post-prefix bytes
    val c3 = Array[Byte](9, 10, 11, 12)           // more post-prefix bytes => suffix should be 9..12
    val seen = c1 ++ c2 ++ c3

    val in =
      streamOfChunks(List(c1, c2, c3)) ++ Stream.raiseError[F](new RuntimeException("boom"))

    val tap = PrefixAndSuffixTap[Byte](prefixLimit, suffixLimit)

    def withTap(f: Stream[F, Byte] => F[(Stream[F, Byte], F[Snapshot[Byte]])]) =
      f(in).flatMap: (out, snap) =>
        val recoveredOut = out.handleErrorWith(_ => Stream.empty)

        for
          outArr <- recoveredOut.compile.to(Array)
          snap   <- snap
          _ <- F.delay:
            assertEquals(outArr.toSeq, seen.toSeq)
            assertSnapshot(
              snap,
              expectedPrefix = seen.take(prefixLimit),
              expectedSuffix = expectedSuffixNoOverlap(seen, prefixLimit, suffixLimit),
              expectedTotal  = seen.length.toLong,
            )
        yield ()

    test("tap"):
      withTap(tap.tap)
    test("tapOnImmutableChunks"):
      withTap(tap.tapOnImmutableChunks)

  nest("downstream failure: emit N bytes but pull a larger chunk; snapshot reflects pulled bytes"):
    val bytes = (0 until 200).map(_.toByte).toArray
    val in    = streamOf(bytes) // single chunk => if downstream pulls once, it pulls all 200

    val prefixLimit = 12
    val suffixLimit =  7
    val n           = 27

    val tap = PrefixAndSuffixTap[Byte](prefixLimit, suffixLimit)

    // Pull exactly one upstream chunk, emit only N bytes from it, then fail.
    def emitNTryPullWholeFirstChunkThenFail(n: Int)(s: Stream[F, Byte]): Stream[F, Byte] =
      s.chunks.pull.uncons1.flatMap:
        case None =>
          Pull.done
        case Some((hd, _tl)) =>
          Pull.output(hd.take(n)) >> Pull.raiseError[F](new RuntimeException("downstream boom"))
      .stream // NOTE: already Stream[F, Byte]; no .unchunks

    def withTap(f: Stream[F, Byte] => F[(Stream[F, Byte], F[Snapshot[Byte]])]) =
      f(in).flatMap: (out, snapF) =>
        val failingOut   = emitNTryPullWholeFirstChunkThenFail(n)(out)
        val recoveredOut = failingOut.handleErrorWith(_ => Stream.empty.covary[F])

        for
          outArr <- recoveredOut.compile.to(Array)
          snap   <- snapF
          _ <- F.delay:
            // emitted bytes are exactly N
            assertEquals(outArr.length, n)
            assertEquals(outArr.toSeq, bytes.take(n).toSeq)

            // but tap observed the whole pulled chunk (200)
            assertEquals(snap.count, bytes.length.toLong)

            // snapshot matches the pulled bytes (all 200 in this test)
            assertEquals(snap.prefix.toArray.toSeq, bytes.take(prefixLimit).toSeq)
            assertEquals(snap.suffix.toArray.toSeq, expectedSuffixNoOverlap(bytes, prefixLimit, suffixLimit).toSeq)
        yield ()

    test("tap"):
      withTap(tap.tap)
    test("tapOnImmutableChunks"):
      withTap(tap.tapOnImmutableChunks)

  nest("external timer: snapshot mid-stream is safe and reflects partial progress (deterministic gating)"):
    val c1 = ( 0 until 10).map(_.toByte).toArray
    val c2 = (10 until 25).map(_.toByte).toArray
    val tap = PrefixAndSuffixTap[Byte](prefixLimit = 8, suffixLimit = 5)

    def withTap(f: Stream[F, Byte] => F[(Stream[F, Byte], F[Snapshot[Byte]])]) =
      for
        gate1 <- Deferred[F, Unit]
        gate2 <- Deferred[F, Unit]
        in =
          Stream.eval(gate1.get) >> streamOf(c1) ++
            (Stream.eval(gate2.get) >> streamOf(c2))

        (out, snap) <- f(in)

        fiber <- out.compile.drain.start

        // before any gate is opened
        s0 <- snap
        _ <- F.delay(assertSnapshot(s0, Array.emptyByteArray, Array.emptyByteArray, 0))

        _ <- gate1.complete(())
        s1 <- waitUntilTotal(snap, expectedAtLeast = 10)
        _ <- F.delay:
          val seen1 = c1
          assertSnapshot(
            s1,
            expectedPrefix = seen1.take(8),
            expectedSuffix = expectedSuffixNoOverlap(seen1, prefixLimit = 8, suffixLimit = 5), // now only bytes 8..9
            expectedTotal = 10,
          )

        _ <- gate2.complete(())
        _ <- fiber.joinWithNever

        s2 <- snap
        _ <- F.delay:
          val seen2 = c1 ++ c2
          assertSnapshot(
            s2,
            expectedPrefix = seen2.take(8),
            expectedSuffix = expectedSuffixNoOverlap(seen2, prefixLimit = 8, suffixLimit = 5),
            expectedTotal = 25,
          )
      yield ()

    test("tap"):
      withTap(tap.tap)
    test("tapOnImmutableChunks"):
      withTap(tap.tapOnImmutableChunks)

  nest("previously obtained snapshots do not change after more bytes are pulled"):
    val c1 = ( 0 until 10).map(_.toByte).toArray
    val c2 = (10 until 20).map(_.toByte).toArray
    val tap = PrefixAndSuffixTap[Byte](prefixLimit = 5, suffixLimit = 5)

    def withTap(f: Stream[F, Byte] => F[(Stream[F, Byte], F[Snapshot[Byte]])]) =
      for
        gate1 <- Deferred[F, Unit]
        gate2 <- Deferred[F, Unit]
        in =
          Stream.eval(gate1.get) >> streamOf(c1) ++
            (Stream.eval(gate2.get) >> streamOf(c2))

        (out, snap) <- f(in)
        fiber <- out.compile.drain.start

        _ <- gate1.complete(())
        s1 <- waitUntilTotal(snap, expectedAtLeast = 10)

        // capture values now (ensure no later mutation can affect them)
        s1Prefix = s1.prefix.toArray.clone()
        s1Suffix  = s1.suffix.toArray.clone()

        _ <- gate2.complete(())
        _ <- fiber.joinWithNever
        s2 <- snap

        _ <- F.delay:
          assertEquals(s1.prefix.toArray.toSeq, s1Prefix.toSeq)
          assertEquals(s1.suffix.toArray.toSeq, s1Suffix.toSeq)
          assertEquals(s2.count, 20)
      yield ()

    test("tap"):
      withTap(tap.tap)
    test("tapOnImmutableChunks"):
      withTap(tap.tapOnImmutableChunks)

  nest("snapshot stability while prefix is still partial (prefix grows later)"):
    val c1 = Array[Byte](0, 1, 2)              // prefixLen = 3 (< 5)
    val c2 = Array[Byte](3, 4, 5, 6, 7, 8, 9)  // prefix becomes full later
    val tap = PrefixAndSuffixTap[Byte](prefixLimit = 5, suffixLimit = 4)

    def withTap(f: Stream[F, Byte] => F[(Stream[F, Byte], F[Snapshot[Byte]])]) =
      for
        gate1 <- Deferred[F, Unit]
        gate2 <- Deferred[F, Unit]
        in =
          Stream.eval(gate1.get) >> streamOf(c1) ++
            (Stream.eval(gate2.get) >> streamOf(c2))

        (out, snap) <- f(in)
        fiber <- out.compile.drain.start

        _ <- gate1.complete(())
        s1 <- waitUntilTotal(snap, expectedAtLeast = c1.length.toLong)

        s1Prefix = s1.prefix.toArray.clone()
        s1Suffix  = s1.suffix.toArray.clone() // should now be empty, but we only assert stability

        _ <- gate2.complete(())
        _ <- fiber.joinWithNever
        s2 <- snap

        _ <- F.delay:
          // old snapshot unchanged
          assertEquals(s1.prefix.toArray.toSeq, s1Prefix.toSeq)
          assertEquals(s1.suffix.toArray.toSeq, s1Suffix.toSeq)

          // final snapshot correct (no-overlap suffix)
          val all = c1 ++ c2
          assertSnapshot(
            s2,
            expectedPrefix = all.take(5),
            expectedSuffix = expectedSuffixNoOverlap(all, prefixLimit = 5, suffixLimit = 4),
            expectedTotal = all.length.toLong
          )
      yield ()

    test("tap"):
      withTap(tap.tap)
    test("tapOnImmutableChunks"):
      withTap(tap.tapOnImmutableChunks)

  nest("suffixLimit edge cases: suffixLimit=1 (and 2)"):
    val bytes = (0 until 10).map(_.toByte).toArray
    val in =
      streamOfChunks(
        List(
          bytes.slice(0, 3),
          bytes.slice(3, 9),
          bytes.slice(9, 10),
        )
      )

    def runBoth(prefixLimit: Int, suffixLimit: Int) =
      val tap = PrefixAndSuffixTap[Byte](prefixLimit, suffixLimit)

      def withTap(f: Stream[F, Byte] => F[(Stream[F, Byte], F[Snapshot[Byte]])]) =
        f(in).flatMap: (out, snap) =>
          out.compile.drain >> snap.map: snap =>
            assertSnapshot(
              snap,
              expectedPrefix = bytes.take(prefixLimit),
              expectedSuffix = expectedSuffixNoOverlap(bytes, prefixLimit, suffixLimit),
              expectedTotal = bytes.length.toLong,
            )

      test(s"tap (suffixLimit=$suffixLimit)"):
        withTap(tap.tap)
      test(s"tapOnImmutableChunks (suffixLimit=$suffixLimit)"):
        withTap(tap.tapOnImmutableChunks)

    runBoth(prefixLimit = 4, suffixLimit = 1)
    runBoth(prefixLimit = 4, suffixLimit = 2)

  nest("chunk.size == suffixLimit replaces suffix exactly after prefix is filled"):
    val prefixLimit = 3
    val suffixLimit = 5

    val c1 = Array[Byte](0, 1, 2)                // fills prefix exactly
    val c2 = Array[Byte](10, 11, 12, 13, 14)     // exactly suffixLimit
    val in = streamOfChunks(List(c1, c2))
    val tap = PrefixAndSuffixTap[Byte](prefixLimit, suffixLimit)

    def withTap(f: Stream[F, Byte] => F[(Stream[F, Byte], F[Snapshot[Byte]])]) =
      f(in).flatMap: (out, snap) =>
        out.compile.drain >> snap.map: snap =>
          val all = c1 ++ c2
          assertSnapshot(
            snap,
            expectedPrefix = all.take(prefixLimit),
            expectedSuffix = c2, // no-overlap => post-prefix bytes are exactly c2
            expectedTotal  = all.length.toLong,
          )

    test("tap"):
      withTap(tap.tap)
    test("tapOnImmutableChunks"):
      withTap(tap.tapOnImmutableChunks)

  nest("stream length == prefixLimit -> suffix is empty (no overlap)"):
    val prefixLimit = 10
    val suffixLimit = 5

    val all = (0 until prefixLimit).map(_.toByte).toArray

    def runScenario(label: String, in: Stream[F, Byte]): Unit =
      val tap = PrefixAndSuffixTap[Byte](prefixLimit, suffixLimit)

      def withTap(f: Stream[F, Byte] => F[(Stream[F, Byte], F[Snapshot[Byte]])]) =
        f(in).flatMap: (out, snap) =>
          out.compile.drain >> snap.map: snap =>
            assertSnapshot(
              snap,
              expectedPrefix = all,
              expectedSuffix = Array.emptyByteArray,
              expectedTotal  = all.length.toLong,
            )

      test(s"$label / tap"):
        withTap(tap.tap)
      test(s"$label / tapOnImmutableChunks"):
        withTap(tap.tapOnImmutableChunks)

    runScenario("single chunk", streamOf(all))
    runScenario("multi-chunk", streamOfChunks(List(all.take(3), all.drop(3))))

  nest("no overlap: suffix stays empty until prefix becomes full (mid-stream snapshots)"):
    val prefixLimit = 8
    val suffixLimit = 5

    val c1 = ( 0 until  3).map(_.toByte).toArray // partial prefix only
    val c2 = ( 3 until 20).map(_.toByte).toArray // completes prefix and provides post-prefix bytes

    val all = c1 ++ c2
    val tap = PrefixAndSuffixTap[Byte](prefixLimit, suffixLimit)

    def withTap(f: Stream[F, Byte] => F[(Stream[F, Byte], F[Snapshot[Byte]])]) =
      for
        gate1 <- Deferred[F, Unit]
        gate2 <- Deferred[F, Unit]

        in =
          (Stream.eval(gate1.get) >> streamOf(c1)) ++
            (Stream.eval(gate2.get) >> streamOf(c2))

        (out, snap) <- f(in)
        fiber <- out.compile.drain.start

        // let c1 through only: prefix is partial -> suffix must be empty
        _ <- gate1.complete(())
        s1 <- waitUntilTotal(snap, expectedAtLeast = c1.length.toLong)
        _ <- F.delay:
          assertEquals(s1.prefix.toArray.toSeq, c1.toSeq)
          assertEquals(s1.suffix.size, 0)
          assertEquals(s1.count, c1.length.toLong)

        // now let c2 through and finish
        _ <- gate2.complete(())
        _ <- fiber.joinWithNever

        sEnd <- snap
        _ <- F.delay:
          assertSnapshot(
            sEnd,
            expectedPrefix = all.take(prefixLimit),
            expectedSuffix = expectedSuffixNoOverlap(all, prefixLimit, suffixLimit),
            expectedTotal  = all.length.toLong,
          )
      yield ()

    test("tap"):
      withTap(tap.tap)
    test("tapOnImmutableChunks"):
      withTap(tap.tapOnImmutableChunks)

  nest("stress: concurrently calling snapshot while draining does not throw / stays consistent"):
    val prefixLimit = 16
    val suffixLimit = 7

    val chunks: Vector[Array[Byte]] =
      (0 until 200).toVector.map: i =>
        (0 until 50).map(j => ((i + j) & 0x7f).toByte).toArray

    val in = streamOfChunks(chunks)
    val tap = PrefixAndSuffixTap[Byte](prefixLimit, suffixLimit)

    def withTap(f: Stream[F, Byte] => F[(Stream[F, Byte], F[Snapshot[Byte]])]) =
      f(in).flatMap: (out, snap) =>
        for
          drainFiber <- out.compile.drain.start

          snapFiber <- {
            def loop(n: Int, lastCount: Long): F[Unit] =
              if n <= 0 then F.unit
              else
                snap.flatMap: s =>
                  F.delay:
                    assert(s.prefix.size >= 0 && s.prefix.size <= prefixLimit)
                    assert(s.suffix.size >= 0 && s.suffix.size <= suffixLimit)
                    assert(s.count >= lastCount)
                  >> F.cede >> loop(n - 1, s.count)

            loop(n = 1000, lastCount = 0L)
          }.start

          _ <- drainFiber.joinWithNever
          _ <- snapFiber.joinWithNever

          finalSnap <- snap
          _ <- F.delay:
            val expectedTotal = chunks.iterator.map(_.length.toLong).sum
            val all = chunks.iterator.flatMap(_.iterator).toArray

            assertEquals(finalSnap.count, expectedTotal)
            assertEquals(finalSnap.prefix.toArray.toSeq, all.take(prefixLimit).toSeq)
            assertEquals(
              finalSnap.suffix.toArray.toSeq,
              expectedSuffixNoOverlap(all, prefixLimit, suffixLimit).toSeq,
            )
        yield ()

    test("tap"):
      withTap(tap.tap)
    test("tapOnImmutableChunks"):
      withTap(tap.tapOnImmutableChunks)

  nest("concurrency stress: suffixLimit=0 branch"):
    val prefixLimit = 16
    val suffixLimit = 0

    val chunks: Vector[Array[Byte]] =
      (0 until 200).toVector.map: i =>
        (0 until 50).map(j => ((i + j) & 0x7f).toByte).toArray

    val in = streamOfChunks(chunks)
    val tap = PrefixAndSuffixTap[Byte](prefixLimit, suffixLimit)

    def withTap(f: Stream[F, Byte] => F[(Stream[F, Byte], F[Snapshot[Byte]])]) =
      f(in).flatMap: (out, snap) =>
        for
          drainFiber <- out.compile.drain.start

          snapFiber <- {
            def loop(n: Int, lastCount: Long): F[Unit] =
              if n <= 0 then F.unit
              else
                snap.flatMap: s =>
                  F.delay:
                    assert(s.prefix.size >= 0 && s.prefix.size <= prefixLimit)
                    assertEquals(s.suffix.size, 0) // critical
                    assert(s.count >= lastCount)
                  >> F.cede >> loop(n - 1, s.count)

            loop(n = 1000, lastCount = 0L)
          }.start

          _ <- drainFiber.joinWithNever
          _ <- snapFiber.joinWithNever

          finalSnap <- snap
          _ <- F.delay:
            val expectedTotal = chunks.iterator.map(_.length.toLong).sum
            val all = chunks.iterator.flatMap(_.iterator).toArray
            assertEquals(finalSnap.count, expectedTotal)
            assertEquals(finalSnap.prefix.toArray.toSeq, all.take(prefixLimit).toSeq)
            assertEquals(finalSnap.suffix.size, 0) // critical
        yield ()

    test("tap"):
      withTap(tap.tap)
    test("tapOnImmutableChunks"):
      withTap(tap.tapOnImmutableChunks)

  nest("tapOnImmutableChunks: prefix is copied immediately when it becomes full (not only at end)"):
    val prefixLimit = 5
    val suffixLimit = 2

    // c1 contributes 3 bytes, c2 contributes 2 bytes -> prefix becomes full after c2
    val c1 = Array[Byte](10, 11, 12)
    val c2 = Array[Byte](13, 14, 15) // only first 2 bytes matter for prefix fill
    val c3 = Array[Byte](16, 17, 18)

    test("prefix snapshot after becoming full does not reflect later mutations of original arrays"):
      val tap = PrefixAndSuffixTap[Byte](prefixLimit, suffixLimit)

      for
        gate1 <- Deferred[F, Unit]
        gate2 <- Deferred[F, Unit]
        gate3 <- Deferred[F, Unit]

        in =
          Stream.eval(gate1.get) >> streamOf(c1) ++
            (Stream.eval(gate2.get) >> streamOf(c2)) ++
            (Stream.eval(gate3.get) >> streamOf(c3))

        (out, snap) <- tap.tapOnImmutableChunks(in)
        fiber <- out.compile.drain.start

        // let c1 through: prefix is partial
        _ <- gate1.complete(())
        _ <- waitUntilTotal(snap, expectedAtLeast = c1.length.toLong)

        // let c2 through: prefix should become full (prefixLimit=5)
        _ <- gate2.complete(())
        sFull <- waitUntilTotal(snap, expectedAtLeast = (c1.length + c2.length).toLong)

        // snapshot right after prefix fullness; capture a deep copy of what prefix is NOW
        p0 = sFull.prefix.toArray.clone()

        // mutate original backing arrays after prefix already full
        _ <- F.delay:
          c1(0) = 99.toByte
          c2(0) = 98.toByte

        // confirm that the *current* snapshot prefix is unchanged (should be copied already)
        sAfterMut <- snap
        _ <- F.delay:
          assertEquals(sAfterMut.prefix.size, prefixLimit)
          assertEquals(sAfterMut.prefix.toArray.toSeq, p0.toSeq)

        // now let the stream proceed and finish (ensures we didn't just freeze everything)
        _ <- gate3.complete(())
        _ <- fiber.joinWithNever

        sEnd <- snap
        _ <- F.delay:
          // prefix stays the same at end, too
          assertEquals(sEnd.prefix.size, prefixLimit)
          assertEquals(sEnd.prefix.toArray.toSeq, p0.toSeq)
      yield ()

  nest("generic O test: Int (ClassTag)"):
    def streamOfIntChunk(xs: Array[Int]): Stream[F, Int] =
      Stream.chunk(Chunk.array(xs))

    def streamOfIntChunks(chunks: Seq[Array[Int]]): Stream[F, Int] =
      Stream.emits(chunks).flatMap(streamOfIntChunk)

    val c1 = Array(1, 2)
    val c2 = Array(3, 4, 5, 6)
    val c3 = Array(7)

    val all = c1 ++ c2 ++ c3
    val in  = streamOfIntChunks(List(c1, c2, c3))

    val prefixLimit = 4
    val suffixLimit = 3
    val tap = PrefixAndSuffixTap[Int](prefixLimit, suffixLimit)

    def withTap(f: Stream[F, Int] => F[(Stream[F, Int], F[Snapshot[Int]])]) =
      f(in).flatMap: (out, snap) =>
        for
          outArr <- out.compile.to(Array)
          snap   <- snap
          _ <- F.delay:
            assertEquals(outArr.toSeq, all.toSeq)
            assertSnapshot(
              snap,
              expectedPrefix = all.take(prefixLimit),
              expectedSuffix = expectedSuffixNoOverlap(all, prefixLimit, suffixLimit),
              expectedTotal  = all.length.toLong,
            )
        yield ()

    test("tap"):
      withTap(tap.tap)
    test("tapOnImmutableChunks"):
      withTap(tap.tapOnImmutableChunks)

  nest("not drained: snapshot stays at zero (no eager pulling)"):
    val inBytes = (0 until 50).map(_.toByte).toArray
    val in      = streamOf(inBytes)
    val tap = PrefixAndSuffixTap[Byte](prefixLimit = 10, suffixLimit = 5)

    def withTap(f: Stream[F, Byte] => F[(Stream[F, Byte], F[Snapshot[Byte]])]) =
      f(in).flatMap: (_out, snap) =>
        // Intentionally do NOT run/drain _out
        for
          s1 <- snap
          _  <- F.cede
          s2 <- snap
          _  <- F.cede
          s3 <- snap
          _  <- F.delay:
            assertSnapshot(s1, Array.emptyByteArray, Array.emptyByteArray, 0L)
            assertSnapshot(s2, Array.emptyByteArray, Array.emptyByteArray, 0L)
            assertSnapshot(s3, Array.emptyByteArray, Array.emptyByteArray, 0L)
        yield ()

    test("tap"):
      withTap(tap.tap)
    test("tapOnImmutableChunks"):
      withTap(tap.tapOnImmutableChunks)

  nest("cancellation: snapshot reflects bytes pulled so far, and remains readable after cancel"):
    val c1 = (0 until 10).map(_.toByte).toArray
    val c2 = (10 until 25).map(_.toByte).toArray
    val c3 = (25 until 40).map(_.toByte).toArray

    val prefixLimit = 8
    val suffixLimit = 5
    val tap = PrefixAndSuffixTap[Byte](prefixLimit, suffixLimit)

    def withTap(f: Stream[F, Byte] => F[(Stream[F, Byte], F[Snapshot[Byte]])]) =
      for
        gate1 <- Deferred[F, Unit]
        gate2 <- Deferred[F, Unit]
        gate3 <- Deferred[F, Unit]

        in =
          Stream.eval(gate1.get) >> streamOf(c1) ++
            (Stream.eval(gate2.get) >> streamOf(c2)) ++
            (Stream.eval(gate3.get) >> streamOf(c3))

        (out, snap) <- f(in)

        fiber <- out.compile.drain.start

        // allow first chunk to be pulled
        _  <- gate1.complete(())
        s1 <- waitUntilTotal(snap, expectedAtLeast = c1.length.toLong)

        // cancel before allowing c2/c3
        _ <- fiber.cancel

        // snapshot should still be callable and should reflect only what was pulled
        sAfter <- snap

        _ <- F.delay:
          val seen = c1 // only c1 should have passed the gate
          val expectedSuffix = expectedSuffixNoOverlap(seen, prefixLimit, suffixLimit) // now only bytes 8..9
          assertSnapshot(
            s1,
            expectedPrefix = seen.take(prefixLimit),
            expectedSuffix = expectedSuffix,
            expectedTotal  = seen.length.toLong,
          )
          assertSnapshot(
            sAfter,
            expectedPrefix = seen.take(prefixLimit),
            expectedSuffix = expectedSuffix,
            expectedTotal  = seen.length.toLong,
          )

        // prove it doesn't "advance" after cancel, even if we open remaining gates
        _ <- gate2.complete(())
        _ <- gate3.complete(())
        _ <- F.cede
        sFinal <- snap
        _ <- F.delay:
          val seen = c1
          assertSnapshot(
            sFinal,
            expectedPrefix = seen.take(prefixLimit),
            expectedSuffix = expectedSuffixNoOverlap(seen, prefixLimit, suffixLimit),
            expectedTotal  = seen.length.toLong,
          )
      yield ()

    test("tap"):
      withTap(tap.tap)
    test("tapOnImmutableChunks"):
      withTap(tap.tapOnImmutableChunks)
