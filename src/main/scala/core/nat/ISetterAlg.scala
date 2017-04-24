package org.hablapps.stateless
package core
package nat

import scalaz.{ Const, Monad, MonadState, ~> }

trait ISetterAlg[P[_], I, A] extends raw.ISetterAlg[P, I, A]
    with IOpticAlg[P, I, A, MonadState, Const[Unit, ?]] {

  def modify(f: A => A): P[Unit] = map(hom(_ => ev.modify(f)))(_.getConst)

  def composeISetter[J, B](st: ISetterAlg[Q, J, B]): ISetterAlg.Aux[P, st.Q, (I, J), B] =
    ISetterAlg(λ[λ[x => ((I, J)) => st.Q[x]] ~> λ[x => P[Const[Unit, x]]]] { iqx =>
      map(hom(i => st.hom(j => iqx((i, j)))))(_ => Const(()))
    })(this, st.ev)

  def composeITraversal[J, B](tr: ITraversalAlg[Q, J, B]): ISetterAlg.Aux[P, tr.Q, (I, J), B] =
    composeISetter(tr.asISetter)

  def composeIOptional[J, B](op: IOptionalAlg[Q, J, B]): ISetterAlg.Aux[P, op.Q, (I, J), B] =
    composeISetter(op.asISetter)

  def composeILens[J, B](ln: ILensAlg[Q, J, B]): ISetterAlg.Aux[P, ln.Q, (I, J), B] =
    composeISetter(ln.asISetter)
}

object ISetterAlg {

  type Aux[P[_], Q2[_], I, A] = ISetterAlg[P, I, A] { type Q[x] = Q2[x] }

  def apply[P[_], Q2[_], I, A](
      hom2: λ[x => I => Q2[x]] ~> λ[x => P[Const[Unit, x]]])(implicit
      ev0: Monad[P],
      ev1: MonadState[Q2, A]): Aux[P, Q2, I, A] = new ISetterAlg[P, I, A] {
    type Q[x] = Q2[x]
    def point[X](x: => X) = ev0.point(x)
    def bind[X, Y](fx: P[X])(f: X => P[Y]): P[Y] = ev0.bind(fx)(f)
    implicit val ev = ev1
    val hom = hom2
  }
}
