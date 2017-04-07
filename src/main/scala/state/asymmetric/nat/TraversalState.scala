package org.hablapps.phoropter
package state
package asymmetric
package nat

import scalaz._, Scalaz._

import monocle.Traversal

import core.asymmetric.nat.TraversalAlg

trait TraversalState {

  def fromTraversal[F[_]: Monad, S, A](
      tr: Traversal[S, A]): TraversalAlg[StateT[F, S, ?], StateT[F, A, ?], A] =
    TraversalAlg[StateT[F, S, ?], StateT[F, A, ?], A](
      new (StateT[F, A, ?] ~> λ[x => StateT[F, S, List[x]]]) {
        def apply[X](sa: StateT[F, A, X]) = StateT { s =>
          tr.modifyF(sa.exec)(s).tuple(tr.getAll(s).traverse(sa.eval))
        }
      })
}
