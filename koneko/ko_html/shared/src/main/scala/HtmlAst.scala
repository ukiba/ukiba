package jp.ukiba.koneko
package ko_html

/**
 * A simple HTML Abstract Syntax Tree.
 *
 * String can contain code points from Unicode Private Use Area
 * when used by the interpolation.
 */
object HtmlAst:
  /** Tag */
  case class Elem(
    name: String,
    attrs: Iterable[Attr] = Nil,
    children: Seq[Elem | String] = Nil,
  )

  case class Attr(
    name: String,
    value: Option[String] = None, // could be String for a performance improvement (None becomes "")
  )
  object Attr:
    def apply(name: String, value: String): Attr = Attr(name, Some(value))

  /** Characters of the destination slot for interpolation arguments */
  inline val ArgCharMin = '\uE000' // Unicode Private Use Area
  inline val ArgCharMax = '\uF8FF'
  inline val MaxArgsLen = ArgCharMax - ArgCharMin + 1 // 6400 code points
