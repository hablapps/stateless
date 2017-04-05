package org.hablapps.phoropter
package state

import scalaz.{ Monad, StateT, ~> }
import StateT.stateTMonadState
import scalaz.syntax.monad._

import monocle.Optional

import core.MonadOptional

trait StateOptional {

  def fromOptional[F[_]: Monad, S, A](
      op: Optional[S, A]): MonadOptional[StateT[F, S, ?], StateT[F, A, ?], A] = {
    new MonadOptional[StateT[F, S, ?], StateT[F, A, ?], A] {

      def point[X](x: => X) = stateTMonadState.point(x)

      def bind[X, Y](fx: StateT[F, S, X])(f: X => StateT[F, S, Y]) =
        stateTMonadState.bind(fx)(f)

      implicit val MS = stateTMonadState

      val hom: StateT[F, A, ?] ~> λ[x => StateT[F, S, Option[x]]] =
        new (StateT[F, A, ?] ~> λ[x => StateT[F, S, Option[x]]]) {
          def apply[X](sa: StateT[F, A, X]): StateT[F, S, Option[X]] =
            StateT(s => op.getOption(s).map(sa.run).fold(
              (s, Option.empty[X]).point[F])(
              _.map { case (a, o) => (op.set(a)(s), Option(o)) }))
        }
    }
  }
}
