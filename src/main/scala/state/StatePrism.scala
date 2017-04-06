package org.hablapps.phoropter
package state

import scalaz.{ Monad, StateT, ~> }
import scalaz.syntax.monad._

import monocle.Prism

import core.PrismAlg

trait StatePrism {

  def fromPrism[F[_]: Monad, S, A](
      pr: Prism[S, A]): PrismAlg[StateT[F, S, ?], StateT[F, A, ?], A] =
    PrismAlg[StateT[F, S, ?], StateT[F, A, ?], A](
      new (StateT[F, A, ?] ~> λ[x => StateT[F, S, Option[x]]]) {
        def apply[X](sa: StateT[F, A, X]): StateT[F, S, Option[X]] =
          StateT(s => pr.getOption(s).map(sa.run).fold(
            (s, Option.empty[X]).point[F])(
            _.map { case (a, o) => (pr.reverseGet(a), Option(o)) }))
      })
}
