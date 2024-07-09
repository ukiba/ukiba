package jp.ukiba.koneko
package ko_ffmpeg

import org.typelevel.log4cats.Logger
import cats.effect.{Sync, Resource}
import cats.syntax.all.*

import scala.jdk.CollectionConverters.*
import java.io.{InputStream, Reader, InputStreamReader, BufferedReader, StringReader}
import java.nio.charset.StandardCharsets.UTF_8

// ported from jip camstream
class ProcessHelper[F[_]](using F: Sync[F], log: Logger[F]):
  case class RunResult(
    exitCode: Int,
    stdout: String,
    stderr: String,
  ):
    def stdoutLines(): Iterator[String] =
      val lineReader = BufferedReader(StringReader(stdout))
      Iterator.continually(lineReader.readLine()).takeWhile(_ != null)

    def stderrLines(): Iterator[String] =
      val lineReader = BufferedReader(StringReader(stderr))
      Iterator.continually(lineReader.readLine()).takeWhile(_ != null)

  def run(command: String*): F[RunResult] =
    for
      _ <- log.debug(s"process start: ${command.mkString(" ")}")
      proc = new ProcessBuilder(command.asJava).start()

      stdoutCapture = CaptureRunnable(proc.getInputStream)
      stderrCapture = CaptureRunnable(proc.getErrorStream)

      stdoutThread = Thread(stdoutCapture)
      stderrThread = Thread(stderrCapture)

      _ = stdoutThread.start()
      _ = stderrThread.start()

      _ = stdoutThread.join()
      _ = stderrThread.join()

      _ = stdoutCapture.thrown.foreach(throw _)
      _ = stderrCapture.thrown.foreach(throw _)

      stdout = stdoutCapture.out.toString
      stderr = stderrCapture.out.toString

      exitCode = proc.waitFor()
      _ <-
        if exitCode != 0 then
          F.raiseError(RuntimeException(s"process exited with $exitCode: ${command.mkString(" ")}" + 
              (if stdout.nonEmpty then s"\nstdout\n$stdout" else "") +
              (if stderr.nonEmpty then s"\nstderr\n$stderr" else "")))
        else if stderr.nonEmpty then
          log.warn(s"process has emitted stderr: ${command.mkString(" ")}\n$stderr")
        else
          //F.unit
          log.warn(s"process has emitted stdout: ${command.mkString(" ")}\n$stdout")

    yield RunResult(exitCode, stdout, stderr)

  class CaptureRunnable(reader: Reader) extends Runnable:
    val out = StringBuilder()
    var thrown: Option[Throwable] = None

    override def run =
      try
        val buf = new Array[Char](1024)
        Iterator.continually(reader.read(buf)).takeWhile(_ != -1).foreach: len =>
          out.appendAll(buf, 0, len)

      catch
        case ex: Throwable =>
          thrown = Some(ex)

  object CaptureRunnable:
    def apply(stream: InputStream): CaptureRunnable = new CaptureRunnable(InputStreamReader(stream, UTF_8))
