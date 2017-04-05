package org.hablapps.phoropter
package state

import scalaz._, Scalaz._

import monocle.Traversal

import core.MonadTraversal

trait StateTraversal {

  def fromTraversal[F[_]: Monad, S, A](
      tr: Traversal[S, A]): MonadTraversal[StateT[F, S, ?], StateT[F, A, ?], A] =
    MonadTraversal[StateT[F, S, ?], StateT[F, A, ?], A](
      new (StateT[F, A, ?] ~> λ[x => StateT[F, S, List[x]]]) {
        def apply[X](sa: StateT[F, A, X]) = StateT { s =>
          tr.modifyF(sa.exec)(s).tuple(tr.getAll(s).traverse(sa.eval))
        }
      })
}
