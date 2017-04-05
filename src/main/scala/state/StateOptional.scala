package org.hablapps.phoropter
package state

import scalaz.{ Monad, StateT, ~> }
import StateT.stateTMonadState
import scalaz.syntax.monad._

import monocle.Optional

import core.MonadOptional

trait StateOptional {

  def fromOptional[F[_]: Monad, S, A](
      op: Optional[S, A]): MonadOptional[StateT[F, S, ?], StateT[F, A, ?], A] =
    MonadOptional[StateT[F, S, ?], StateT[F, A, ?], A](
      new (StateT[F, A, ?] ~> λ[x => StateT[F, S, Option[x]]]) {
        def apply[X](sa: StateT[F, A, X]): StateT[F, S, Option[X]] =
          StateT(s => op.getOption(s).map(sa.run).fold(
            (s, Option.empty[X]).point[F])(
            _.map { case (a, o) => (op.set(a)(s), Option(o)) }))
      })
}
