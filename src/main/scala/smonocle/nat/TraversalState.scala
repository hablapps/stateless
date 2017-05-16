package org.hablapps.stateless
package smonocle
package nat

import scalaz._, Scalaz._

import monocle.{ Traversal => MTraversal }

import core.nat.TraversalAlg

trait TraversalState {

  type Traversal[S, A] = TraversalAlg.Aux[State[S, ?], State[A, ?], A]

  implicit def asTraversal[S, A](tr: MTraversal[S, A]): Traversal[S, A] =
    fromTraversal[Id, S, A](tr)

  def fromTraverse[F[_]: Monad, T[_]: Traverse, A]
      : TraversalAlg.Aux[StateT[F, T[A], ?], StateT[F, A, ?], A] =
    TraversalAlg[StateT[F, T[A], ?], StateT[F, A, ?], A](
      λ[StateT[F, A, ?] ~> λ[x => StateT[F, T[A], List[x]]]] { sa =>
        StateT(_.traverse(sa.run).map { tax =>
          (tax.map(_._1), tax.foldMap(tp => List(tp._2)))
        })
      })

  def fromTraversal[F[_]: Monad, S, A](
      tr: MTraversal[S, A]): TraversalAlg.Aux[StateT[F, S, ?], StateT[F, A, ?], A] =
    TraversalAlg[StateT[F, S, ?], StateT[F, A, ?], A](
      new (StateT[F, A, ?] ~> λ[x => StateT[F, S, List[x]]]) {
        def apply[X](sa: StateT[F, A, X]) = StateT { s =>
          tr.modifyF(sa.exec)(s).tuple(tr.getAll(s).traverse(sa.eval))
        }
      })
}
