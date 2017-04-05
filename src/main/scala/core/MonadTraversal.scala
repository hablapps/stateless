package org.hablapps.phoropter
package core

import scalaz.{ Monad, MonadState, ~> }

trait MonadTraversal[P[_], Q[_], A] extends Monad[P] {

  implicit val MS: MonadState[Q, A]
  val hom: Q ~> λ[x => P[List[x]]]

  /* derived algebra */

  def getAll: P[List[A]] = hom(MS.get)

  def set(a: A): P[Unit] = map(hom(MS.put(a)))(_ => ())

  def modify(f: A => A): P[Unit] = map(hom(MS.modify(f)))(_ => ())
}

object MonadTraversal {

  def apply[P[_], Q[_], A](
      hom2: Q ~> λ[x => P[List[x]]])(implicit
      ev0: Monad[P],
      ev1: MonadState[Q, A]) = new MonadTraversal[P, Q, A] {
    def point[X](x: => X) = ev0.point(x)
    def bind[X, Y](fx: P[X])(f: X => P[Y]): P[Y] = ev0.bind(fx)(f)
    implicit val MS = ev1
    val hom = hom2
  }
}
