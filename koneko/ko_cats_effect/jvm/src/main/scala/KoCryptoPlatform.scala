package jp.ukiba.koneko
package ko_cats
package effect

import cats.effect.Async
import cats.syntax.all.*

import java.security.MessageDigest

abstract class KoCryptoPlatform:
  def hash[F[_]: Async](algo: String)(bytes: Array[Byte]): F[Array[Byte]] =
    Async[F].delay:
      MessageDigest.getInstance(algo).digest(bytes)
