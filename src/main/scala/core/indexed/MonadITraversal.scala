package org.hablapps.phoropter
package core
package indexed

import scalaz.{ Monad, MonadState, ~> }
import scalaz.syntax.functor._

trait MonadITraversal[P[_], Q[_], I, A] extends Monad[P] {

  implicit val MS: MonadState[Q, A]
  val hom: λ[x => I => Q[x]] ~> λ[x => P[List[x]]]

  /* derived algebra */

  def getAll: P[List[(I, A)]] = hom(MS.get.strengthL)

  def set(a: A): P[Unit] = map(hom(_ => MS.put(a)))(_ => ())

  def modify(f: A => A): P[Unit] = map(hom(_ => MS.modify(f)))(_ => ())

  def indexes: P[List[I]] = hom(MS.point(_))

  def foci: P[List[A]] = hom(_ => MS.get)

  def collect[O](qo: Q[O]): P[List[O]] = hom(_ => qo)
}

object MonadITraversal {

  def apply[P[_], Q[_], I, A](
      hom2: λ[x => I => Q[x]] ~> λ[x => P[List[x]]])(implicit
      ev0: Monad[P],
      ev1: MonadState[Q, A]) = new MonadITraversal[P, Q, I, A] {
    def point[X](x: => X) = ev0.point(x)
    def bind[X, Y](fx: P[X])(f: X => P[Y]): P[Y] = ev0.bind(fx)(f)
    implicit val MS = ev1
    val hom = hom2
  }
}
