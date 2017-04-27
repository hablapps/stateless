package org.hablapps.stateless
package smonocle
package nat

import scalaz._, Scalaz._

import shapeless._

import monocle.{ Traversal => MTraversal }

import core.nat.ITraversalAlg

trait ITraversalState {

  def mapITraversal[F[_]: Monad, K, V]
      : ITraversalAlg.Aux[StateT[F, Map[K, V], ?], StateT[F, V, ?], K :: HNil, V] =
    ITraversalAlg[StateT[F, Map[K, V], ?], StateT[F, V, ?], K :: HNil, V](
      λ[λ[x => K :: HNil => StateT[F, V, x]] ~> λ[x => StateT[F, Map[K, V], List[x]]]] { sx =>
        StateT(_.toList
          .traverse { case (k, v) => sx(k :: HNil)(v).strengthL(k) }
          .map(xs => (xs.map { case (k, (v, x)) => ((k, v), x) }).unzip)
          .map { case (kvs, os) => (kvs.toMap, os) })
      })
}
