package jp.ukiba.koneko
package ko_fs2
package io
package file
package syntax

import ko_munit.KoCatsEffectSuite
import ko_fs2.io.file.syntax.all.*

import fs2.io.file.Files
import fs2.Stream
import cats.syntax.all.*

import scala.util.Random
import java.io.IOException //java.nio.file.NoSuchFileException is not available on Scala.js

class FilesSyntaxTests extends KoCatsEffectSuite:
  val random = Random()

  nest("createParentDirectoryIfNotExists"):
    test("writeAll"):
      for
        tmpDir <- Files[F].createTempDirectory
        file = tmpDir.resolve("a/b")
        bytes = random.nextBytes(256 * 1024)

        // fails without the parent directory
        _ <- Stream.emits(bytes).through(Files[F].writeAll(file))
            .compile.drain.intercept[IOException]

        // succeeds with createParentIfNotExists
        _ <- Stream.emits(bytes).through(Files[F].createParentIfNotExists.writeAll(file))
            .compile.drain

        _ <- Files[F].readAll(file).compile.to(Seq)
            .assertEquals(bytes.toSeq)

        _ <- Files[F].deleteRecursively(tmpDir)
      yield ()

    test("writeCursor"):
      for
        tmpDir <- Files[F].createTempDirectory
        file = tmpDir.resolve("a/b")
        bytes = random.nextBytes(256 * 1024 * 10)

        // fails without the parent directory
        _ <- Files[F].writeCursor(file).use: writeCursor =>
          writeCursor.writeAll(Stream.emits(bytes))
              .void.stream.compile.drain
        .intercept[IOException]

        // succeeds with createParentIfNotExists
        _ <- Files[F].createParentIfNotExists.writeCursor(file).use: writeCursor =>
          writeCursor.writeAll(Stream.emits(bytes).chunkLimit(1024).unchunks)
              .void.stream.compile.drain

        _ <- Files[F].readAll(file).compile.to(Seq).
            assertEquals(bytes.toSeq)

        _ <- Files[F].deleteRecursively(tmpDir)
      yield ()

    test("writeUtf8"):
      for
        tmpDir <- Files[F].createTempDirectory
        file = tmpDir.resolve("a/b")
        text = random.nextString(256 * 1024)

        // fails without the parent directory
        _ <- Stream.emit(text).through(Files[F].writeUtf8(file))
            .compile.drain.intercept[IOException]

        // succeeds with createParentIfNotExists
        _ <- Stream.emit(text).through(Files[F].createParentIfNotExists.writeUtf8(file))
            .compile.drain

        _ <- Files[F].readUtf8(file).compile.string
            .assertEquals(text)

        _ <- Files[F].deleteRecursively(tmpDir)
      yield ()

    test("writeUtf8Lines"):
      for
        tmpDir <- Files[F].createTempDirectory
        file = tmpDir.resolve("a/b")
        texts = (0 until 256).map(_ => random.alphanumeric.take(1024).mkString).toList

        // fails without the parent directory
        _ <- Stream.emits(texts).through(Files[F].writeUtf8Lines(file))
            .compile.drain.intercept[IOException]

        // succeeds with createParentIfNotExists
        _ <- Stream.emits(texts).through(Files[F].createParentIfNotExists.writeUtf8Lines(file))
            .compile.drain

        _ <- Files[F].readUtf8Lines(file).compile.to(Seq)
            .map(_.dropRight(1)) // the last element is empty
            .assertEquals(texts)

        _ <- Files[F].deleteRecursively(tmpDir)
      yield ()

  nest("appender"):
    nest("Array[Byte]"):
      test("Parallel"):
        for
          tmpDir <- Files[F].createTempDirectory
          file = tmpDir.resolve("a/b")
          texts = (0 until 1024).map(_ => random.alphanumeric.take(1024).mkString).toList

          // fails without the parent directory
          _ <- Files[F].appender(file).use: appender =>
            texts.parTraverse: text =>
              appender.write(text)
          .intercept[IOException]

          // succeeds with createParentIfNotExists
          _ <- Files[F].createParentIfNotExists.appender(file).use: appender =>
            texts.parTraverse: text =>
              appender.write(text)

          _ <- Files[F].size(file)
              .assertEquals(texts.map(_.size).sum)

          _ <- Files[F].deleteRecursively(tmpDir)
        yield ()

    nest("String"):
      test("Sequential"):
        for
          tmpDir <- Files[F].createTempDirectory
          file = tmpDir.resolve("a/b")
          texts = (0 until 1024).map(_ => random.alphanumeric.take(1024).mkString).toList

          // fails without the parent directory
          _ <- Files[F].appender(file).use: appender =>
            texts.traverse: text =>
              appender.write(text)
          .intercept[IOException]

          // succeeds with createParentIfNotExists
          _ <- Files[F].createParentIfNotExists.appender(file).use: appender =>
            texts.traverse: text =>
              appender.write(text)

          _ <- Files[F].readUtf8(file).compile.string
              .assertEquals(texts.mkString)

          _ <- Files[F].deleteRecursively(tmpDir)
        yield ()
