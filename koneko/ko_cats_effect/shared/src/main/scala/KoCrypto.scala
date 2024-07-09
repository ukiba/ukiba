package jp.ukiba.koneko
package ko_cats
package effect

import cats.effect.Async
import cats.syntax.all.*

import java.nio.charset.StandardCharsets, StandardCharsets.UTF_8

// [TSec](https://github.com/jmcardon/tsec/commits/master/) doesn't seem to be maintained

object KoCrypto extends KoCryptoPlatform:
  object sha2:
    extension (str: String) 
      inline def sha256[F[_]: Async]: F[Array[Byte]] = hash("SHA-256"    )(str.getBytes(UTF_8))
