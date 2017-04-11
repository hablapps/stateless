package org.hablapps.stateless
package core
package asymmetric
package nat

import scalaz.{ Monad, MonadState, ~> }
import scalaz.syntax.functor._

trait ITraversalAlg[P[_], Q[_], I, A] extends raw.ITraversalAlg[P, I, A]
    with IOpticAlg[P, Q, I, A, MonadState, List] {

  def getList: P[List[(I, A)]] = hom(ev.get.strengthL)

  def modifyList(f: A => A): P[List[Unit]] = hom(_ => ev.modify(f))

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
