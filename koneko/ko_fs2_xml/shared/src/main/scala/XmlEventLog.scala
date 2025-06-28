package jp.ukiba.koneko
package ko_fs2
package xml

import fs2.data.xml.XmlEvent
import fs2.{Stream, Pipe}
import org.typelevel.log4cats.Logger

object XmlEventLog:
  def pipe[F[_]]/*(using log: Logger[F])*/: Pipe[F, XmlEvent, XmlEvent] =
    _.map: ev =>
      /*log.debug*/println(s"ev = $ev")
      ev

