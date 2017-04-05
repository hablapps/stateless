package org.hablapps.phoropter
package state

import scalaz.{ Monad, StateT, ~> }
import StateT.stateTMonadState
import scalaz.syntax.functor._

import monocle.Lens

import core.MonadSetter

trait StateSetter {

  // XXX: we can't create a `MonadSetter` from a `Getter`, because we need an
  // `A` somehow. The only way to access an `A` with `Setter` is by means of
  // `setter.modify`, that requires an `A => A` as input. However, that function
  // ignores the attached output once applied `State[A, ?]` over the element
  // `A`, so we are using a `Lens` instead.
  def fromSetter[F[_]: Monad, S, A](
      ln: Lens[S, A]): MonadSetter[StateT[F, S, ?], StateT[F, A, ?], A] = {
    new MonadSetter[StateT[F, S, ?], StateT[F, A, ?], A] {

      def point[X](x: => X) = stateTMonadState.point(x)

      def bind[X, Y](fx: StateT[F, S, X])(f: X => StateT[F, S, Y]) =
        stateTMonadState.bind(fx)(f)

      implicit val MS = stateTMonadState

      val hom = λ[StateT[F, A, ?] ~> StateT[F, S, ?]] {
        sa => StateT(s => sa.xmap(ln.set(_)(s))(ln.get)(s))
      }
    }
  }
}
