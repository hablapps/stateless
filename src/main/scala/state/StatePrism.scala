package org.hablapps.phoropter
package state

import scalaz.{ Monad, StateT, ~> }
import StateT.stateTMonadState
import scalaz.syntax.monad._

import monocle.Prism

import core.MonadPrism

trait StatePrism {

  def fromPrism[F[_]: Monad, S, A](
      pr: Prism[S, A]): MonadPrism[StateT[F, S, ?], StateT[F, A, ?], A] =
    MonadPrism[StateT[F, S, ?], StateT[F, A, ?], A](
      new (StateT[F, A, ?] ~> λ[x => StateT[F, S, Option[x]]]) {
        def apply[X](sa: StateT[F, A, X]): StateT[F, S, Option[X]] =
          StateT(s => pr.getOption(s).map(sa.run).fold(
            (s, Option.empty[X]).point[F])(
            _.map { case (a, o) => (pr.reverseGet(a), Option(o)) }))
      })
}
