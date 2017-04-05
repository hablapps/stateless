package org.hablapps.phoropter
package core
package indexed

import scalaz.{ Monad, MonadReader, ~> }
import scalaz.syntax.functor._

trait MonadIGetter[P[_], Q[_], I, A] extends Monad[P] {

  implicit val MR: MonadReader[Q, A]
  val hom: λ[x => I => Q[x]] ~> P

  /* derived algebra */

  def get: P[(I, A)] = hom(MR.ask.strengthL)
}
