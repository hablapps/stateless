package org.hablapps.phoropter
package core

import scalaz.{ MonadReader, ~> }

trait MonadGetter[P[_], Q[_], A] extends MonadReader[P, A] {

  implicit val MR: MonadReader[Q, A]
  val hom: Q ~> P // monad homomorphism

  /* derived algebra */

  def get: P[A] = hom(MR.ask)
  
  override def ask = get
}
