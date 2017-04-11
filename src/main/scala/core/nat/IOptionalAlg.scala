package org.hablapps.stateless
package core
package asymmetric
package nat
package indexed

import scalaz.{ Monad, MonadState, ~> }
import scalaz.Id.Id
import scalaz.syntax.functor._

trait IOptionalAlg[P[_], Q[_], I, A] extends raw.indexed.IOptionalAlg[P, I, A]
    with IOpticAlg[P, Q, I, A, MonadState, Option] {

  def getOption: P[Option[(I, A)]] = hom(ev.get.strengthL)

  def setOption(a: A): P[Option[Unit]] = hom(_ => ev.put(a))
}

object IOptionalAlg {

  def apply[P[_], Q[_], I, A](
      hom2: λ[x => I => Q[x]] ~> λ[y => P[Option[y]]])(implicit
      ev0: Monad[P],
      ev1: MonadState[Q, A]) = new IOptionalAlg[P, Q, I, A] {
    def point[X](x: => X) = ev0.point(x)
    def bind[X, Y](fx: P[X])(f: X => P[Y]): P[Y] = ev0.bind(fx)(f)
    implicit val ev = ev1
    val hom = hom2
  }
}
