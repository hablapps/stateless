package org.hablapps.phoropter
package state

import scalaz.{ State, ~> }
import scalaz.syntax.functor._
import scalaz.std.tuple._

import monocle.Prism

import core.MonadPrism

trait StatePrism {

  def fromPrism[S, A](pr: Prism[S, A]): MonadPrism[State[S, ?], State[A, ?], A] = {
    new MonadPrism[State[S, ?], State[A, ?], A] {

      def point[X](x: => X) = stateMonad.point(x)

      def bind[X, Y](fx: State[S, X])(f: X => State[S, Y]) =
        stateMonad.bind(fx)(f)

      implicit val MS = stateMonad

      val hom: State[A, ?] ~> λ[x => State[S, Option[x]]] =
        new (State[A, ?] ~> λ[x => State[S, Option[x]]]) {
          def apply[X](sa: State[A, X]): State[S, Option[X]] =
            State(s => pr.getOption(s).map(sa.run).fold((s, Option.empty[X])) {
              case (a, o) => (pr.reverseGet(a), Option(o))
            })
        }
    }
  }
}
