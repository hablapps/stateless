package org.hablapps.phoropter
package core
package asymmetric
package nat

import scalaz.{ Monad, ~> }

trait OpticAlg2[P[_], Q[_], A, Ev[M[_], _] <: Monad[M], F[_]]
    extends indexed.IOpticAlg[P, Q, Unit, A, Ev, F] {

  val hom: λ[x => Unit => Q[x]] ~> λ[x => P[F[x]]] =
    λ[λ[x => Unit => Q[x]] ~> λ[x => P[F[x]]]](fx => xhom(fx(())))

  val xhom: Q ~> λ[x => P[F[x]]]
}

trait OpticAlg[P[_], Q[_], A, Ev[M[_], _] <: Monad[M], F[_]] extends Monad[P] {

  implicit val ev: Ev[Q, A]

  val hom: Q ~> λ[x => P[F[x]]]
}
