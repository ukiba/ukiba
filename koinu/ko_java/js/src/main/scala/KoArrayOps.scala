package jp.ukiba.koinu

import scala.scalajs.js, js.typedarray.{ArrayBuffer, Int8Array, byteArray2Int8Array}

object KoArrayOps:
  extension (array: Array[Byte])
    def toInt8Array: Int8Array = byteArray2Int8Array(array)

  extension (arrayBuffer: ArrayBuffer)
    def toArray: Array[Byte] = Int8Array(arrayBuffer).toArray
