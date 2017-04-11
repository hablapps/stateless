package org.hablapps.stateless
package core
package nat

import scalaz.{ Monad, MonadReader, ~> }
import scalaz.syntax.functor._

trait IFoldAlg[P[_], Q[_], I, A] extends raw.IFoldAlg[P, I, A]
    with IOpticAlg[P, Q, I, A, MonadReader, List] {

  def getList: P[List[(I, A)]] = hom(ev.ask.strengthL)
}

object IFoldAlg {

  def apply[P[_], Q[_], I, A](
      hom2: λ[x => I => Q[x]] ~> λ[x => P[List[x]]])(implicit
      ev0: Monad[P],
      ev1: MonadReader[Q, A]) = new IFoldAlg[P, Q, I, A] {
    def point[X](x: => X) = ev0.point(x)
    def bind[X, Y](fx: P[X])(f: X => P[Y]): P[Y] = ev0.bind(fx)(f)
    implicit val ev = ev1
    val hom = hom2
  }
}
