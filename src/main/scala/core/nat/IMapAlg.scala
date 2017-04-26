package org.hablapps.stateless
package core
package nat

import scalaz.{ Monad, MonadState, ~> }
import scalaz.syntax.functor._
import scalaz.syntax.id._

import op._, At.syntax._

trait IMapAlg[P[_], R[_], I, A] extends raw.IMapAlg[P, I, A]
    with IOpticAlg[P, I, A, MonadState, List] {

  implicit val ta: At.Aux[P, R, I, A]

  /* derived algebra */

  def getList: P[List[(I, A)]] = hom(ev.get.strengthL)

  def modifyList(f: A => A): P[List[Unit]] = hom(_ => ev.modify(f))

  def updateOption(i: I)(oa: Option[A]): P[Unit] =
    ta.at(i) |> (ln => ln.hom(ln.ev.put(oa)))

  /* new algebra */

  def collect[O](qo: Q[O]): P[List[O]] = hom(_ => qo)

  def pick[O](i: I)(ro: R[O]): P[O] = ta.at(i).hom[O](ro)
}

object IMapAlg {

  type Aux[P[_], Q2[_], R[_], I, A] = IMapAlg[P, R, I, A] { type Q[x] = Q2[x] }

  def apply[P[_], Q2[_], R[_], I, A](
      hom2: λ[x => I => Q2[x]] ~> λ[x => P[List[x]]])(implicit
      ev0: Monad[P],
      ev1: MonadState[Q2, A],
      ev2: At.Aux[P, R, I, A]): Aux[P, Q2, R, I, A] = new IMapAlg[P, R, I, A] {
    type Q[x] = Q2[x]
    def point[X](x: => X) = ev0.point(x)
    def bind[X, Y](fx: P[X])(f: X => P[Y]): P[Y] = ev0.bind(fx)(f)
    implicit val ev = ev1
    val hom = hom2
    implicit val ta = ev2
  }
}
