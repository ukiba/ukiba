package jp.ukiba.koinu.ko_java

import scala.collection.mutable
import scala.util.Properties
import java.io.{PrintStream, OutputStream}
import java.nio.charset.Charset
import java.time.{ZoneId, Instant, Duration}
import java.util.Locale
import java.lang.management.{ManagementFactory, MemoryUsage}

/** Helpers for the application process */
object KoAppProcessHelper {
  val pid = ManagementFactory.getRuntimeMXBean.getPid
  def processStartedMessage = s"pid $pid: started"

  val started = Instant.now
  def uptime = Duration.between(started, Instant.now)
  def processExitingMessage = s"pid $pid: exiting: ${uptimeMessageOf(uptime)}"

  def processRuntimeMessage: String = {
    import Properties.{javaVmVendor, javaVmName, javaVmVersion, javaVmInfo, osName}
    Seq(
      s"java: $javaVmVersion, $javaVmVendor, $javaVmName, $javaVmInfo",
      s"java default: timeZone = ${ZoneId.systemDefault}, charset = ${Charset.defaultCharset}, locale = ${Locale.getDefault}",
      s"os: $osName ${Properties.propOrNull("os.version")} (${Properties.propOrNull("os.arch")}), ${Runtime.getRuntime.availableProcessors} processors",
    ).mkString("\n")
  }

  implicit class MemoryUsageOps(val mem: MemoryUsage) extends AnyVal {
    def toText: String =
      f"${mem.getUsed / 1024 / 1024}%,dMB used / ${mem.getCommitted / 1024 / 1024}%,d committed${mem.getMax match {
        case  -1 => "" // undefined
        case   0 => "" // Java 17: getNonHeapMemoryUsage.getMax seems to be always 0
        case max => f" / ${max / 1024 / 1024} max"
      }}"
  }

  def memoryAndThreadUsageMessage: String = {
    val memoryBean = ManagementFactory.getMemoryMXBean
    val heap = memoryBean.getHeapMemoryUsage
    val nonHeap = memoryBean.getNonHeapMemoryUsage

    val threadBean = ManagementFactory.getThreadMXBean
    val threadCount = threadBean.getThreadCount
    val daemonThreadCount = threadBean.getDaemonThreadCount

    s"heap memory ${heap.toText} (nonHeap ${nonHeap.toText}), ${threadCount - daemonThreadCount} threads + $daemonThreadCount daemons, ${uptimeMessageOf(uptime)}"
  }

  def uptimeMessageOf(duration: Duration): String =
      f"up ${duration.toDaysPart}%d days ${duration.toHoursPart}%02d:${duration.toMinutesPart}%02d:${duration.toSecondsPart}%02d"
}
