package org.hablapps.phoropter
package state

import scalaz.{ State, ~> }
import scalaz.syntax.functor._
import scalaz.std.tuple._

import monocle.Lens

import core.MonadLens

trait StateLens {

  def fromLens[S, A](ln: Lens[S, A]): MonadLens[State[S, ?], State[A, ?], A] = {
    new MonadLens[State[S, ?], State[A, ?], A] {

      def point[X](x: => X) = stateMonad.point(x)

      def bind[X, Y](fx: State[S, X])(f: X => State[S, Y]) =
        stateMonad.bind(fx)(f)

      implicit val MS = stateMonad

      val hom: State[A, ?] ~> State[S, ?] = λ[State[A, ?] ~> State[S, ?]] {
        sa => State(s => sa.xmap(ln.set(_)(s))(ln.get)(s))
      }

      def init = get
    }
  }
}
