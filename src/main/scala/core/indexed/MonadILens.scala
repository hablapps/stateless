package org.hablapps.phoropter
package core
package indexed

import scalaz.{ Monad, MonadState, ~> }
import scalaz.syntax.functor._

trait MonadILens[P[_], Q[_], I, A] extends Monad[P] {

  implicit val MS: MonadState[Q, A]
  val hom: λ[x => I => Q[x]] ~> P

  /* derived algebra */

  def get: P[(I, A)] = hom(MS.get.strengthL)

  def put(a: A): P[Unit] = hom(_ => MS.put(a))
}
