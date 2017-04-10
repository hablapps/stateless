package org.hablapps.phoropter
package smonocle
package asymmetric
package nat

import scalaz.{ Monad, StateT, ~> }
import scalaz.syntax.monad._

import monocle.Optional

import core.asymmetric.nat.OptionalAlg

trait OptionalState {

  def fromOptional[F[_]: Monad, S, A](
      op: Optional[S, A]): OptionalAlg[StateT[F, S, ?], StateT[F, A, ?], A] =
    OptionalAlg[StateT[F, S, ?], StateT[F, A, ?], A](
      new (StateT[F, A, ?] ~> λ[x => StateT[F, S, Option[x]]]) {
        def apply[X](sa: StateT[F, A, X]): StateT[F, S, Option[X]] =
          StateT(s => op.getOption(s).map(sa.run).fold(
            (s, Option.empty[X]).point[F])(
            _.map { case (a, o) => (op.set(a)(s), Option(o)) }))
      })
}
