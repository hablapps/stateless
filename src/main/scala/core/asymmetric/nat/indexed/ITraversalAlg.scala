package org.hablapps.phoropter
package core
package asymmetric
package nat
package indexed

import scalaz.{ Monad, MonadState, ~> }
import scalaz.syntax.functor._

trait ITraversalAlg[P[_], Q[_], I, A] extends IOpticAlg[P, Q, I, A, MonadState, List] {

  def getAll: P[List[(I, A)]] = hom(ev.get.strengthL)

  def set(a: A): P[Unit] = map(hom(_ => ev.put(a)))(_ => ())

  def modify(f: A => A): P[Unit] = map(hom(_ => ev.modify(f)))(_ => ())

  def indexes: P[List[I]] = hom(ev.point(_))

  def foci: P[List[A]] = hom(_ => ev.get)

  def collect[O](qo: Q[O]): P[List[O]] = hom(_ => qo)
}

object ITraversalAlg {

  def apply[P[_], Q[_], I, A](
      hom2: λ[x => I => Q[x]] ~> λ[x => P[List[x]]])(implicit
      ev0: Monad[P],
      ev1: MonadState[Q, A]) = new ITraversalAlg[P, Q, I, A] {
    def point[X](x: => X) = ev0.point(x)
    def bind[X, Y](fx: P[X])(f: X => P[Y]): P[Y] = ev0.bind(fx)(f)
    implicit val ev = ev1
    val hom = hom2
  }
}
