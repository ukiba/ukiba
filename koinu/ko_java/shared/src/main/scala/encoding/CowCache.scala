package jp.ukiba.koinu.ko_java
package encoding

/**
 * A copy on write cache
 * 1. with lock free reads,
 * 2. updates with overhead,
 * 3. performs well with low number of entries (8-16ish)
 */
trait CowCache[K, V]:
  def of(key: K): V

object CowCache:
  /*
   * @typeparam K must have stable `equals`/`hashCode` for the lifetime of the entry
   * @param computeValue called at most once per `K` unless it throws.
   *                     has to be idempotent, and must not call `of` on the same instance.
   */
  def ofMap[K, V](computeValue: K => V): OfMap[K, V] = OfMap(computeValue)
  final class OfMap[K, V](computeValue: K => V)  // final class can help the JIT optimize call sites
      extends CowCache[K, V]:
    @volatile private var map: Map[K, V] = Map.empty
    private final val lock = new AnyRef

    final def of(key: K): V =
      map.get(key) match  // lock free reads
        case Some(value) => value
        case None =>  // this should be rare
          lock.synchronized:
            val mapSnap = map  // avoid multiple volatile reads
            mapSnap.get(key) match  // getOrElse could be used but might not be inlined
              case Some(value) => value  // another thread already inserted
              case None =>
                // `computeValue` inside lock holds the lock longer but avoids multiple `computeValue` calls
                val value = computeValue(key)
                map = mapSnap.updated(key, value)  // replace the whole map
                value

  /*
   * @typeparam K2 must have stable `equals`/`hashCode` for the lifetime of the entry
   * @param computeMapKey when the same `K2` is produced for different `K`, the same `V` will be returned.
   *                      must be stable for a given `K`
   * @param computeValue called at most once per `K2` unless it throws.
   *                     has to be idempotent, and must not call `of` on the same instance.
   */
  def ofMapWithKeyTransform[K, K2, V](computeMapKey: K => K2)(computeValue: K => V): OfMapWithKeyTransform[K, K2, V]
      = OfMapWithKeyTransform(computeMapKey)(computeValue)
  final class OfMapWithKeyTransform[K, K2, V]  // final class can help the JIT optimize call sites
      (computeMapKey: K => K2)(computeValue: K => V)
      extends CowCache[K, V]:
    @volatile private var map: Map[K2, V] = Map.empty
    private final val lock = new AnyRef

    final def of(key: K): V =
      val mapKey = computeMapKey(key)
      map.get(mapKey) match  // lock free reads
        case Some(value) => value
        case None =>  // this should be rare
          lock.synchronized:
            val mapSnap = map  // avoid multiple volatile reads
            mapSnap.get(mapKey) match  // getOrElse could be used but might not be inlined
              case Some(value) => value  // another thread already inserted
              case None =>
                // `computeValue` inside lock holds the lock longer but avoids multiple `computeValue` calls
                val value = computeValue(key)
                map = mapSnap.updated(mapKey, value)  // replace the whole map
                value
