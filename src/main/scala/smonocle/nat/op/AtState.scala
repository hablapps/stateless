package org.hablapps.stateless
package smonocle
package nat
package op

import scalaz._, Scalaz._

import core.nat.LensAlg
import core.nat.op.At

trait AtState extends AtStateInstances {

  implicit def fromAtState[K, V]
      : At[State[Map[K, V], ?], State[Option[V], ?], K, V] =
    new At[State[Map[K, V], ?], State[Option[V], ?], K, V] {
      def at(k: K) = LensAlg[State[Map[K, V], ?], State[Option[V], ?], Option[V]](
        λ[State[Option[V], ?] ~> State[Map[K, V], ?]] { sx =>
          State(s => sx(s.get(k)).swap.map(_.fold(s - k)(p => s + (k -> p))).swap)
        })
    }
}

trait AtStateInstances {

  implicit def fromAtStateT[F[_]: Monad, K, V]
      : At[StateT[F, Map[K, V], ?], StateT[F, Option[V], ?], K, V] =
    new At[StateT[F, Map[K, V], ?], StateT[F, Option[V], ?], K, V] {
      def at(k: K) = LensAlg[StateT[F, Map[K, V], ?], StateT[F, Option[V], ?], Option[V]](
        λ[StateT[F, Option[V], ?] ~> StateT[F, Map[K, V], ?]] { sx =>
          StateT(s => sx(s.get(k)).map(_.swap.map(_.fold(s - k)(p => s + (k -> p))).swap))
        })
    }

  // XXX: this feels like composing lens with `At`, so probably could be taken
  // to a more generic setting instead of here.
  implicit def fromAtStateT[F[_]: Monad, S, K, V](
      ln: LensAlg.Aux[StateT[F, S, ?], StateT[F, Map[K, V], ?], Map[K, V]])
      : At[StateT[F, S, ?], StateT[F, Option[V], ?], K, V] =
    new At[StateT[F, S, ?], StateT[F, Option[V], ?], K, V] {
      def at(k: K) = ln.composeLens(fromAtStateT[F, K, V].at(k))
    }
}

object atState extends AtState
