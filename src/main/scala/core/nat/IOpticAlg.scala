package org.hablapps.stateless
package core
package nat

import scalaz.{ Monad, ~> }

trait IOpticAlg[P[_], I, A, Ev[M[_], _] <: Monad[M], F[_]] extends Monad[P] {

  type Q[_]

  implicit val ev: Ev[Q, A]

  val hom: λ[x => I => Q[x]] ~> λ[x => P[F[x]]]
}
