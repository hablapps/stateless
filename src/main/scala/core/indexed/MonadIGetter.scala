package org.hablapps.phoropter
package core
package indexed

import scalaz.{ Monad, MonadReader, ~> }

trait MonadIGetter[P[_], Q[_], I, A] extends Monad[P] {

  implicit val MR: MonadReader[Q, (I, A)]
  val hom: Q ~> P // monad homomorphism

  /* derived algebra */

  def get: P[(I, A)] = hom(MR.ask)
}
