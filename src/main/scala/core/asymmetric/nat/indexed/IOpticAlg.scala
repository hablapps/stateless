package org.hablapps.phoropter
package core
package asymmetric
package nat
package indexed

import scalaz.{ Monad, ~> }

trait IOpticAlg[P[_], Q[_], I, A, Ev[M[_], _] <: Monad[M], F[_]] extends Monad[P] {

  implicit val ev: Ev[Q, A]

  val hom: λ[x => I => Q[x]] ~> λ[x => P[F[x]]]
}
