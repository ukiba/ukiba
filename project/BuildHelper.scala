import sbt.util.Logger

import scala.sys.process.Process
import scala.util.Properties.isWin

object npm {
  val npmCommand = if (isWin) Seq("cmd", "/c", "npm") else Seq("npm") // Windows: npm.cmd has to be run by cmd 

  def apply(args: String*)(implicit log: Logger) {
    val command = npmCommand ++ args
    log.info(s"exec: ${command.mkString(" ")}")
    Process(command).run.exitValue match {
      case 0 =>
      case exitCode => throw new Exception(s"exitCode is $exitCode")
    }   
  }
}
