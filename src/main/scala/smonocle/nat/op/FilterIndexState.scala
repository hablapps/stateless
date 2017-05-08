package org.hablapps.stateless
package smonocle
package nat
package op

import scalaz._, Scalaz._

import shapeless._

import core.nat.{ LensAlg, ITraversalAlg }
import core.nat.op.FilterIndex

trait FilterIndexState {

  implicit def fromFilterIndexStateT[F[_]: Monad, K, V]
      : FilterIndex.Aux[StateT[F, Map[K, V], ?], StateT[F, V, ?], K, V] =
    new FilterIndex[StateT[F, Map[K, V], ?], K, V] {
      type Q[x] = StateT[F, V, x]
      def filterIndex(p: K => Boolean) =
        ITraversalAlg[StateT[F, Map[K, V], ?], StateT[F, V, ?], K :: HNil, V](
          new (λ[x => (K :: HNil) => StateT[F, V, x]] ~> λ[x => StateT[F, Map[K, V], List[x]]]) {
            def apply[X](sx: (K :: HNil) => StateT[F, V, X]): StateT[F, Map[K, V], List[X]] = {
              StateT(s => s.foldLeft((Map.empty[K, V], List.empty[X]).point[F])(
                (acc, kv) => acc >>= {
                  case (m, l) if p(kv._1) => sx(kv._1 :: HNil)(kv._2).map {
                    case (v, o) => (m + (kv._1 -> v), l :+ o)
                  }
                  case (m, l) => (m + kv, l).point[F]
                }
              ))
            }
          })
    }

  implicit def fromFilterIndexStateT[F[_]: Monad, S, K, V](
      ln: LensAlg.Aux[StateT[F, S, ?], StateT[F, Map[K, V], ?], Map[K, V]])
      : FilterIndex.Aux[StateT[F, S, ?], StateT[F, V, ?], K, V] =
    new FilterIndex[StateT[F, S, ?], K, V] {
      type Q[x] = StateT[F, V, x]
      def filterIndex(p: K => Boolean) =
        ln.asIndexed.composeTraversal(fromFilterIndexStateT[F, K, V].filterIndex(p))
    }
}

object filterIndexState extends FilterIndexState
