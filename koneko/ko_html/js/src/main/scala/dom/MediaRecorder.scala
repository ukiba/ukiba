package jp.ukiba.koneko
package ko_html
package dom

import org.scalajs.dom.{MediaStream, Blob, Event, EventTarget, EventInit, DOMException}

import scala.scalajs.js
import scala.scalajs.js.annotation.JSGlobal

// Generated by ChatGPT o1-preview
// Write Scala.js facade like scakajs-dom for MediaRecorder

/** Facade for the MediaRecorder API */
@js.native
@JSGlobal
class MediaRecorder(stream: MediaStream, options: js.UndefOr[MediaRecorderOptions] = js.undefined)
    extends EventTarget {

  /** Returns the MIME type selected as the container for the recorded media. */
  val mimeType: String = js.native

  /** Returns the current state of the MediaRecorder object ("inactive", "recording", or "paused"). */
  val state: String = js.native

  /** Returns the MediaStream being recorded. */
  //val stream: MediaStream = js.native

  /** Returns the video encoding bit rate in bits per second. */
  val videoBitsPerSecond: js.UndefOr[Double] = js.native

  /** Returns the audio encoding bit rate in bits per second. */
  val audioBitsPerSecond: js.UndefOr[Double] = js.native

  /** Indicates whether the MediaRecorder should record input when the input media stream is muted. */
  var ignoreMutedMedia: Boolean = js.native

  /** Event handler for the 'start' event. */
  var onstart: js.Function1[Event, Any] = js.native

  /** Event handler for the 'stop' event. */
  var onstop: js.Function1[Event, Any] = js.native

  /** Event handler for the 'dataavailable' event. */
  var ondataavailable: js.Function1[BlobEvent, Any] = js.native

  /** Event handler for the 'pause' event. */
  var onpause: js.Function1[Event, Any] = js.native

  /** Event handler for the 'resume' event. */
  var onresume: js.Function1[Event, Any] = js.native

  /** Event handler for the 'error' event. */
  var onerror: js.Function1[MediaRecorderErrorEvent, Any] = js.native

  /** Pauses the recording. */
  def pause(): Unit = js.native

  /** Requests a Blob containing the current data. */
  def requestData(): Unit = js.native

  /** Resumes a paused recording. */
  def resume(): Unit = js.native

  /** Starts recording media. Optionally, you can specify the timeslice parameter to record in chunks. */
  def start(timeslice: js.UndefOr[Double] = js.undefined): Unit = js.native

  /** Stops recording. */
  def stop(): Unit = js.native
}

/** Companion object for MediaRecorder, providing static methods. */
@js.native
@JSGlobal
object MediaRecorder extends js.Object {

  /** Checks if the MIME type specified is supported by the current user agent. */
  def isTypeSupported(mimeType: String): Boolean = js.native
}

/** Options for configuring the MediaRecorder. */
trait MediaRecorderOptions extends js.Object {

  /** The desired MIME type for the recording. */
  var mimeType: js.UndefOr[String] = js.undefined

  /** The audio encoding bit rate in bits per second. */
  var audioBitsPerSecond: js.UndefOr[Double] = js.undefined

  /** The video encoding bit rate in bits per second. */
  var videoBitsPerSecond: js.UndefOr[Double] = js.undefined

  /** The total encoding bit rate in bits per second (audio + video). */
  var bitsPerSecond: js.UndefOr[Double] = js.undefined
}

/** Represents a blob of media data. */
@js.native
@JSGlobal
class BlobEvent(`type`: String, init: js.UndefOr[BlobEventInit]) extends Event(`type`, init) {

  /** The Blob data. */
  val data: Blob = js.native

  /** The time at which the data was recorded, relative to the start of recording. */
  val timecode: Double = js.native
}

/** Initialization options for BlobEvent. */
trait BlobEventInit extends EventInit {

  /** The Blob data to be included in the event. */
  var data: Blob

  /** The timecode associated with the data. */
  var timecode: js.UndefOr[Double] = js.undefined
}

/** Represents an error event from the MediaRecorder. */
@js.native
@JSGlobal
class MediaRecorderErrorEvent(`type`: String, init: js.UndefOr[MediaRecorderErrorEventInit]) extends Event(`type`, init) {

  /** The DOMException representing the error. */
  val error: DOMException = js.native
}

/** Initialization options for MediaRecorderErrorEvent. */
trait MediaRecorderErrorEventInit extends EventInit {

  /** The DOMException representing the error. */
  var error: DOMException
}

