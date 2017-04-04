package org.hablapps.phoropter
package state

import scalaz.{ State, ~> }
import scalaz.syntax.functor._
import scalaz.std.tuple._

import monocle.Optional

import core.MonadOptional

trait StateOptional {

  def fromOptional[S, A](op: Optional[S, A]): MonadOptional[State[S, ?], State[A, ?], A] = {
    new MonadOptional[State[S, ?], State[A, ?], A] {

      def point[X](x: => X) = stateMonad.point(x)

      def bind[X, Y](fx: State[S, X])(f: X => State[S, Y]) =
        stateMonad.bind(fx)(f)

      implicit val MS = stateMonad

      val hom: State[A, ?] ~> λ[x => State[S, Option[x]]] =
        new (State[A, ?] ~> λ[x => State[S, Option[x]]]) {
          def apply[X](sa: State[A, X]): State[S, Option[X]] =
            State(s => op.getOption(s).map(sa.run).fold((s, Option.empty[X])) {
              case (a, o) => (op.set(a)(s), Option(o))
            })
        }
    }
  }
}
