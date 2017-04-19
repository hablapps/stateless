package org.hablapps.stateless
package smonocle
package nat

import scalaz.{ Monad, State, StateT, ~> }
import scalaz.Id.Id
import scalaz.syntax.monad._
import scalaz.syntax.std.option._

import monocle.{ Prism => MPrism }

import core.nat.PrismAlg

trait PrismState {

  type PrismField[P[_], A] = PrismAlg[P, State[A, ?], A]

  type Prism[S, A] = PrismAlg[State[S, ?], State[A, ?], A]

  def fromPrism[F[_]: Monad, S, A](
      pr: MPrism[S, A]): PrismAlg[StateT[F, S, ?], StateT[F, A, ?], A] =
    PrismAlg[StateT[F, S, ?], StateT[F, A, ?], A](
      new (StateT[F, A, ?] ~> λ[x => StateT[F, S, Option[x]]]) {
        def apply[X](sa: StateT[F, A, X]): StateT[F, S, Option[X]] =
          StateT(s => pr.getOption(s).map(sa.run).fold(
            (s, Option.empty[X]).point[F])(
            _.map { case (a, x) => (pr.reverseGet(a), x.some) }))
      })

  implicit def asPrismAlg[S, A](pr: MPrism[S, A]): Prism[S, A] =
    fromPrism[Id, S, A](pr)
}
