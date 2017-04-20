package org.hablapps.stateless
package smonocle
package nat

import scalaz.{ Monad, State, StateT, ~> }
import scalaz.Id.Id
import scalaz.syntax.monad._

import monocle.{ Optional => MOptional }

import core.nat.OptionalAlg

trait OptionalState {

  type Optional[S, A] = OptionalAlg[State[S, ?], State[A, ?], A]

  implicit def asOptional[S, A](op: MOptional[S, A]): Optional[S, A] =
    fromOptional[Id, S, A](op)

  def fromOptional[F[_]: Monad, S, A](
      op: MOptional[S, A]): OptionalAlg[StateT[F, S, ?], StateT[F, A, ?], A] =
    OptionalAlg[StateT[F, S, ?], StateT[F, A, ?], A](
      new (StateT[F, A, ?] ~> λ[x => StateT[F, S, Option[x]]]) {
        def apply[X](sa: StateT[F, A, X]): StateT[F, S, Option[X]] =
          StateT(s => op.getOption(s).map(sa.run).fold(
            (s, Option.empty[X]).point[F])(
            _.map { case (a, o) => (op.set(a)(s), Option(o)) }))
      })
}
