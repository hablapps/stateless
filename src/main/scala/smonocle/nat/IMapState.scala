package org.hablapps.stateless
package smonocle
package nat

import scalaz._, Scalaz._

// import core.nat.IMapAlg
import op.atState._

trait IMapState {

  // def fromMap[F[_]: Monad, K, V]: IMapAlg[StateT[F, Map[K, V], ?], StateT[F, V, ?], StateT[F, Option[V], ?], K, V] =
  //   IMapAlg[StateT[F, Map[K, V], ?], StateT[F, V, ?], StateT[F, Option[V], ?], K, V](
  //     λ[λ[x => K => StateT[F, V, x]] ~> λ[x => StateT[F, Map[K, V], List[x]]]] { sx =>
  //       StateT(_.toList
  //         .traverse { case (k, v) => sx(k)(v).strengthL(k) }
  //         .map(xs => (xs.map { case (k, (v, x)) => ((k, v), x) }).unzip)
  //         .map { case (kvs, os) => (kvs.toMap, os) })
  //     })
}
