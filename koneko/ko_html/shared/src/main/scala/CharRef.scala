package jp.ukiba.koneko
package ko_html

import java.lang.Long.parseLong

object CharRef:
  def namedOrThrow(text: String): String =
    named.getOrElse(text, throw IllegalArgumentException(s"Unsupported character reference: &$text;")).toString

  // named character references from
  // https://developer.mozilla.org/en-US/docs/Glossary/Character_reference
  val named = Map[String, Char](
    "amp"   -> '&',
    "lt"    -> '<',
    "gt"    -> '>',
    "quot"  -> '"',
    "apos"  -> '\'',
    "nbsp"  -> '\u00A0', // No-Break Space
    "ndash" -> '–',
    "mdash" -> '—',
    "copy"  -> '©',
    "reg"   -> '®',
    "trade" -> '™',
    "asymp" -> '≈',
    "ne"    -> '≠',
    "pound" -> '£',
    "euro"  -> '€',
    "deg"   -> '°',
  )
/*
  // runMain jp.ukiba.koneko.ko_html.CharRef
  def main(args: Array[String]): Unit =
    named.foreach: (name, ch) =>
      println(f"&$name;\t$ch\t\\u${ch.toInt}%04X")
*/

  // should we support all the named character references from the spec?
  // https://html.spec.whatwg.org/multipage/named-characters.html#named-character-references
  // those without trailing semicolon for legacy compatibility could be dropped

  inline def decimal(text: String): String = numeric(parseLong(text))
  inline def hex(text: String): String = numeric(parseLong(text, 16))
  def numeric(codePoint: Long): String =
    if codePoint <= 0xFFFF then
      String(Array[Char](codePoint.toChar))
    else
      String(Array[Char]((codePoint >> 32 & 0xFFFF).toChar, (codePoint & 0xFFFF).toChar))
