package org.hablapps.stateless
package smonocle
package asymmetric
package nat
package op

import scalaz._, Scalaz._

import core.asymmetric.nat.LensAlg
import core.asymmetric.nat.op.At

trait StateAt {

  implicit def fromIMapState[F[_]: Monad, K, V]
      : At[StateT[F, Map[K, V], ?], StateT[F, Option[V], ?], K, V] =
    new At[StateT[F, Map[K, V], ?], StateT[F, Option[V], ?], K, V] {
      def at(k: K) = LensAlg[StateT[F, Map[K, V], ?], StateT[F, Option[V], ?], Option[V]](
        λ[StateT[F, Option[V], ?] ~> StateT[F, Map[K, V], ?]] { sx =>
          StateT(s => sx(s.get(k)).map(_.swap.map(_.fold(s - k)(p => s + (k -> p))).swap))
        })
    }
}

object atState extends StateAt
