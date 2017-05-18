package org.hablapps.stateless
package core
package nat

import scalaz.{ Equal, Functor, Monad, ~> }
import scalaz.syntax.equal._
import scalaz.syntax.functor._

trait OpticAlg[P[_], A, Ev[M[_], _] <: Monad[M], F[_]] extends Monad[P] { self =>

  type Q[_]

  implicit val ev: Ev[Q, A]
  implicit val fev: Functor[F]

  val hom: Q ~> λ[x => P[F[x]]]

  trait OpticAlgLaw {

    def natural[A, B](
        qa: Q[A])(
        f: A => B)(implicit
        E: Equal[P[F[B]]]): Boolean =
      hom(qa map f) === self.compose(fev).map(hom(qa))(f)
  }
}
