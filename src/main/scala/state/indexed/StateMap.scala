package org.hablapps.phoropter
package state
package indexed

import scalaz._, Scalaz._

import op.all._
import core.indexed.MonadMap

trait StateMap {

  def fromMap[F[_]: Monad, K, V]: MonadMap[StateT[F, Map[K, V], ?], StateT[F, V, ?], StateT[F, Option[V], ?], K, V] =
    MonadMap[StateT[F, Map[K, V], ?], StateT[F, V, ?], StateT[F, Option[V], ?], K, V](
      λ[λ[x => K => StateT[F, V, x]] ~> λ[x => StateT[F, Map[K, V], List[x]]]] { sx =>
        StateT(_.toList
          .traverse { case (k, v) => sx(k)(v).strengthL(k) }
          .map(xs => (xs.map { case (k, (v, x)) => ((k, v), x) }).unzip)
          .map { case (kvs, os) => (kvs.toMap, os) })
      })
}
