package org.hablapps.stateless
package core
package nat

import scalaz.{ Monad, ~> }
import shapeless.HList

trait IOpticAlg[P[_], I <: HList, A, Ev[M[_], _] <: Monad[M], F[_]] extends Monad[P] {

  type Q[_]

  implicit val ev: Ev[Q, A]

  val hom: λ[x => I => Q[x]] ~> λ[x => P[F[x]]]
}
