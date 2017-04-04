package org.hablapps.phoropter
package core

import scalaz.{ Monad, MonadState, ~> }

trait MonadTraversal[P[_], Q[_], A] extends Monad[P] {

  implicit val MS: MonadState[Q, A]
  val hom: Q ~> λ[x => P[List[x]]]

  /* derived algebra */

  def getAll: P[List[A]] = hom(MS.get)
  // def setAll(xs: List[A]): P[Unit] = ???

  def set(a: A): P[Unit] = map(hom(MS.put(a)))(_ => ())
  def modify(f: A => A): P[Unit] = map(hom(MS.modify(f)))(_ => ())
}
