package org.hablapps.phoropter
package state

import scalaz.{ Monad, StateT, ~> }
import StateT.stateTMonadState

import monocle.Lens

import core.MonadLens

trait StateLens {

  def fromLens[F[_]: Monad, S, A](
      ln: Lens[S, A]): MonadLens[StateT[F, S, ?], StateT[F, A, ?], A] = {
    new MonadLens[StateT[F, S, ?], StateT[F, A, ?], A] {

      def point[X](x: => X) = stateTMonadState.point(x)

      def bind[X, Y](p: StateT[F, S, X])(f: X => StateT[F, S, Y]) =
        stateTMonadState.bind(p)(f)

      def init = hom(MS.get)

      implicit val MS = stateTMonadState

      val hom = λ[StateT[F, A, ?] ~> StateT[F, S, ?]] {
        sa => StateT(s => sa.xmap(ln.set(_)(s))(ln.get)(s))
      }
    }
  }
}
