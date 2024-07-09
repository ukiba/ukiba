// TODO migrate to Scala 3 syntax when evip-cloud does

package jp.ukiba.koneko
package ko_cats
package effect

import cats.effect.kernel.{Temporal, Deferred}
import cats.syntax.all._

object KoMapCache {
  type Shard[F[_], K, V, A] = KoAtomicCell[F, Map[K, KoCache[F, V, A]]]

  /** Like [[cats.effect.std.MapRef]] */
  def ofShardedImmutableMap[F[_]: Temporal, K, V, A](shardCount: Int): F[KoMapCache[F, K, V, A]] = {
    for {
      shards <- List.fill(shardCount)(()).traverse { _ =>
        for {
          // AtomicCell uses Mutex, plus Ref when not Async
          cell <- KoAtomicCell[F].of(Map.empty[K, KoCache[F, V, A]])
        } yield cell
      }
    } yield new KoMapCache(shards.toArray)
  }
}
import KoMapCache._

/**
 * Unlike [[cats.effect.std.MapRef]],
 * 1. the Map value is [[KoCache]] rahther than `Option[V]`, and
 * 2. uses [[cats.effect.std.Mutex]] lock to prevent concurrent modification.
 */
class KoMapCache[F[_]: Temporal, K, V, A](
  shards: Array[Shard[F, K, V, A]],
) {
  /**
   * Conditionally update then get the updated value.
   * @param f a function that produces Some to update
   */
/*
  def evalUpdateAndGet(key: K)(init: Init)(conditionalUpdate: ConditionalUpdate): F[Result] = ???
    val shard = shards(key.## / shards.length)
    for {
      cache <- shard.evalModifyConditionally { prevMap =>
        prevMap.get(key) match {
          case Some(cache) => (None, cache).pure[F]
          case None => for {
            cache <- KoCache.of[F, V, A]
            newMap = prevMap + (key -> cache)
          } yield (Some(newMap), cache)
        }
      }

      // FIXME remove asInstanceOf
      result <- cache.evalUpdateAndGet(init.asInstanceOf[cache.Init])(conditionalUpdate.asInstanceOf[cache.ConditionalUpdate])
    } yield result
  }
*/
}
