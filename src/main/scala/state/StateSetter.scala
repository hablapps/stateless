package org.hablapps.phoropter
package state

import scalaz.{ State, ~> }
import scalaz.syntax.functor._
import scalaz.std.tuple._

import monocle.Setter

import core.MonadSetter

trait StateSetter {

  def fromSetter[S, A](st: Setter[S, A]): MonadSetter[State[S, ?], State[A, ?], A] = {
    new MonadSetter[State[S, ?], State[A, ?], A] {

      def point[X](x: => X) = stateMonad.point(x)

      def bind[X, Y](fx: State[S, X])(f: X => State[S, Y]) =
        stateMonad.bind(fx)(f)

      implicit val MS = stateMonad

      // FIXME: can't implement `hom` in terms of `Setter` & `State`
      val hom: State[A, ?] ~> State[S, ?] = λ[State[A, ?] ~> State[S, ?]] {
        sa => State(s => (st.modify(sa.exec)(s), ???))
      }
    }
  }
}
