package org.hablapps.stateless
package core
package nat

import scalaz.{ Monad, MonadReader, ~> }
import scalaz.Id.Id
import scalaz.syntax.functor._

trait IGetterAlg[P[_], Q[_], I, A] extends raw.IGetterAlg[P, I, A]
    with IOpticAlg[P, Q, I, A, MonadReader, Id] {

  def get: P[(I, A)] = hom(ev.ask.strengthL)
}

object IGetterAlg {

  def apply[P[_], Q[_], I, A](
      hom2: λ[x => I => Q[x]] ~> P)(implicit
      ev0: Monad[P],
      ev1: MonadReader[Q, A]) = new IGetterAlg[P, Q, I, A] {
    def point[X](x: => X) = ev0.point(x)
    def bind[X, Y](fx: P[X])(f: X => P[Y]): P[Y] = ev0.bind(fx)(f)
    implicit val ev = ev1
    val hom = hom2
  }
}
