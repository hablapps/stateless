package org.hablapps.phoropter
package core

import scalaz.{ Monad, MonadReader, ~> }

trait MonadFold[P[_], Q[_], A] extends Monad[P] {

  implicit val MR: MonadReader[Q, A]
  val hom: Q ~> λ[x => P[List[x]]]

  /* derived algebra */

  def getAll: P[List[A]] = hom(MR.ask)
}
