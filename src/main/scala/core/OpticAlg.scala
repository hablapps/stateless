package org.hablapps.phoropter
package core

import scalaz.{ Monad, ~> }

trait OpticAlg[P[_], Q[_], A, Ev[_[_], _], F[_]] {

  implicit val ev: Ev[Q, A]

  val hom: Q ~> λ[x => P[F[x]]]
}
