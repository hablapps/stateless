package org.hablapps.phoropter
package core

import scalaz.{ MonadState, ~> }

trait MonadLens[P[_], Q[_], A] extends MonadState[P, A] {

  implicit val MS: MonadState[Q, A]
  val hom: Q ~> P

  /* derived algebra */

  override def get: P[A] = hom(MS.get)

  def set(a: A): P[Unit] = hom(MS.put(a))

  override def put(a: A): P[Unit] = set(a)
}
