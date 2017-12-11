package org.hablapps.stateless
package core
package nat

import scalaz.{ Foldable, Functor, Monad, MonadReader, MonadState, Monoid, ~> }
import scalaz.syntax.foldable._

trait OpticAlg[P[_], A, Ev[M[_], _] <: Monad[M], F[_]] extends Monad[P] {

  type Q[_]

  implicit val ev: Ev[Q, A]

  implicit val fev: Functor[F]

  val hom: Q ~> λ[x => P[F[x]]]
}
