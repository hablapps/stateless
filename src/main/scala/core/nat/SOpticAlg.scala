package org.hablapps.stateless
package core
package nat

import scalaz.{ Monad, ~> }

trait SOpticAlg[P[_], A, B, Ev[M[_], _] <: Monad[M], F[_]] extends Monad[P] {

  type L[_]
  type R[_]

  implicit val evL: Ev[L, A]
  implicit val evR: Ev[R, B]

  val homL: L ~> λ[x => P[F[x]]]
  val homR: R ~> λ[x => P[F[x]]]
}
