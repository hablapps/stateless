package org.hablapps.stateless
package smonocle
package nat
package op

import scalaz._, Scalaz._

import core.nat.LensAlg
import core.nat.op.At

trait AtState extends AtStateInstances {

  implicit def fromIMapState[K, V]
      : At[State[Map[K, V], ?], State[Option[V], ?], K, V] =
    new At[State[Map[K, V], ?], State[Option[V], ?], K, V] {
      def at(k: K) = LensAlg[State[Map[K, V], ?], State[Option[V], ?], Option[V]](
        λ[State[Option[V], ?] ~> State[Map[K, V], ?]] { sx =>
          State(s => sx(s.get(k)).swap.map(_.fold(s - k)(p => s + (k -> p))).swap)
        })
    }
}

trait AtStateInstances {

  implicit def fromIMapState2[F[_]: Monad, K, V]
      : At[StateT[F, Map[K, V], ?], StateT[F, Option[V], ?], K, V] =
    new At[StateT[F, Map[K, V], ?], StateT[F, Option[V], ?], K, V] {
      def at(k: K) = LensAlg[StateT[F, Map[K, V], ?], StateT[F, Option[V], ?], Option[V]](
        λ[StateT[F, Option[V], ?] ~> StateT[F, Map[K, V], ?]] { sx =>
          StateT(s => sx(s.get(k)).map(_.swap.map(_.fold(s - k)(p => s + (k -> p))).swap))
        })
    }
}

object atState extends AtState
