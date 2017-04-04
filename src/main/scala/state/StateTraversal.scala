package org.hablapps.phoropter
package state

import scalaz.{ State, ~> }
import scalaz.syntax.traverse._
import scalaz.std.list._

import monocle.Traversal

import core.MonadTraversal

trait StateTraversal {

  def fromTraversal[S, A](tr: Traversal[S, A]): MonadTraversal[State[S, ?], State[A, ?], A] = {
    new MonadTraversal[State[S, ?], State[A, ?], A] {

      def point[X](x: => X) = stateMonad.point(x)

      def bind[X, Y](fx: State[S, X])(f: X => State[S, Y]) =
        stateMonad.bind(fx)(f)

      implicit val MS = stateMonad

      val hom: State[A, ?] ~> λ[x => State[S, List[x]]] =
        new (State[A, ?] ~> λ[x => State[S, List[x]]]) {
          def apply[X](sa: State[A, X]) = State(s => (
            tr.modify(sa.exec)(s),
            tr.getAll(s).traverse(sa.eval)))
        }
    }
  }
}
