package jp.ukiba.koneko
package ko_fs2
package xml

import fs2.data.xml.XmlEvent
import fs2.{Stream, Pipe}

/**
 * Trims the XML texts, and removes empty text.
 * Intended to be used after `fs2.data.xml.normalize`.
 */
object XmlTextTrimmer:
  def pipe[F[_]]: Pipe[F, XmlEvent, XmlEvent] =
    _.flatMap:
      case XmlEvent.XmlString(str, isCdata) =>
        str.trim match
          case str if str.nonEmpty => Stream.emit(XmlEvent.XmlString(str, false))
          case _                   => Stream.empty

      case ev => Stream.emit(ev)
