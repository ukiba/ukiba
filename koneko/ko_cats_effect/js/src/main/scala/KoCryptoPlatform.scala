package jp.ukiba.koneko
package ko_cats
package effect

import jp.ukiba.koinu.KoArrayOps.{toInt8Array, toArray}

import cats.effect.Async
import cats.syntax.all.*
import org.scalajs.dom

import scala.scalajs.js, js.typedarray.Int8Array

abstract class KoCryptoPlatform:
  def hash[F[_]: Async](algo: String)(bytes: Array[Byte]): F[Array[Byte]] =
    for
      arrayBuffer <- Async[F].fromPromise:
        Async[F].delay:
          dom.crypto.subtle.digest(algo, bytes.toInt8Array).asInstanceOf[js.Promise[js.typedarray.ArrayBuffer]]
    yield
      arrayBuffer.toArray
