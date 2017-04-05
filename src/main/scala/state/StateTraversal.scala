package org.hablapps.phoropter
package state

import scalaz._, Scalaz._
import StateT.stateTMonadState

import monocle.Traversal

import core.MonadTraversal

trait StateTraversal {

  def fromTraversal[F[_]: Monad, S, A](
      tr: Traversal[S, A]): MonadTraversal[StateT[F, S, ?], StateT[F, A, ?], A] = {
    new MonadTraversal[StateT[F, S, ?], StateT[F, A, ?], A] {

      def point[X](x: => X) = stateTMonadState.point(x)

      def bind[X, Y](fx: StateT[F, S, X])(f: X => StateT[F, S, Y]) =
        stateTMonadState.bind(fx)(f)

      implicit val MS = stateTMonadState

      val hom: StateT[F, A, ?] ~> λ[x => StateT[F, S, List[x]]] =
        new (StateT[F, A, ?] ~> λ[x => StateT[F, S, List[x]]]) {
          def apply[X](sa: StateT[F, A, X]) = StateT { s =>
            tr.modifyF(sa.exec)(s).tuple(tr.getAll(s).traverse(sa.eval))
          }
        }
    }
  }
}
