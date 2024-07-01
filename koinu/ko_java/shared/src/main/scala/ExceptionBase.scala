package jp.ukiba.koinu.ko_java

/**
 * A base class to extend java.lang.Exception from Scala.
 *
 * Java 21: cause is always initialized because there are no constructor that
 * leaves cause uninitialized and takes enableSuppression and writableStackTrace at the same time.
 */
abstract class ExceptionBase(message: String, cause: Throwable = null,
    enableSuppression: Boolean = true, writableStackTrace: Boolean = true)
    extends Exception(message, cause, enableSuppression, writableStackTrace):
  def this(cause: Throwable) = this(if cause != null then cause.getMessage else null, cause)
