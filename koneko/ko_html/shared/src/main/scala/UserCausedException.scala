package jp.ukiba.koneko
package ko_html

import jp.ukiba.koinu.ko_java.ExceptionBase

class UserCausedException(
  val message: String, // shown to the user, don't include a sesnsitive information
  val cause: Throwable = null,

  val title: Option[String] = None, // for a error dialog
  val debugDetail: Option[String] = None, // to be logged
) extends ExceptionBase(message, cause)
