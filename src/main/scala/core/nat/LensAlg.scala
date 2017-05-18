package org.hablapps.stateless
package core
package nat

import scalaz.{ Const, Equal, Functor, Monad, MonadState, ~> }
import scalaz.Id.Id
import scalaz.syntax.std.option._
import scalaz.syntax.monad._
import scalaz.syntax.equal._

import shapeless.HNil

trait LensAlg[P[_], A] extends OpticAlg[P, A, MonadState, Id]
    with raw.LensAlg[P, A] {

  override def get: P[A] = hom[A](ev.get)

  override def put(a: A): P[Unit] = hom(ev.put(a))

  /* composing algebras */

  def composeFold[B](fl: FoldAlg[Q, B]): FoldAlg.Aux[P, fl.Q, B] =
    asFold.composeFold(fl)

  def composeGetter[B](gt: GetterAlg[Q, B]): GetterAlg.Aux[P, gt.Q, B] =
    asGetter.composeGetter(gt)

  def composeSetter[B](st: SetterAlg[Q, B]): SetterAlg.Aux[P, st.Q, B] =
    asSetter.composeSetter(st)

  def composeTraversal[B](tr: TraversalAlg[Q, B]): TraversalAlg.Aux[P, tr.Q, B] =
    asTraversal.composeTraversal(tr)

  def composeOptional[B](op: OptionalAlg[Q, B]): OptionalAlg.Aux[P, op.Q, B] =
    asOptional.composeOptional(op)

  def composeLens[B](ln: LensAlg[Q, B]): LensAlg.Aux[P, ln.Q, B] =
    LensAlg(hom compose ln.hom)(this, ln.ev)

  def parLens(ln: LensAlg.Aux[P, Q, A]): TraversalAlg.Aux[P, Q, A] =
    TraversalAlg[P, Q, A](
      λ[Q ~> λ[x => P[List[x]]]] { qx =>
        bind(hom(qx))(a1 => map(ln.hom(qx))(a2 => List(a1, a2)))
      })(this, ev)

  /* transforming algebras */

  def asGetter: GetterAlg.Aux[P, Q, A] = GetterAlg(hom)(this, ev)

  def asOptional: OptionalAlg.Aux[P, Q, A] =
    OptionalAlg(λ[Q ~> λ[x => P[Option[x]]]](qx => map(hom(qx))(_.some)))(this, ev)

  def asFold: FoldAlg.Aux[P, Q, A] = asGetter.asFold

  def asTraversal: TraversalAlg.Aux[P, Q, A] = asOptional.asTraversal

  def asSetter: SetterAlg.Aux[P, Q, A] = asTraversal.asSetter

  def asIndexed: ILensAlg.Aux[P, Q, HNil, A] =
    ILensAlg(new (λ[x => HNil => Q[x]] ~> P) {
      def apply[X](iqx: HNil => Q[X]): P[X] = hom[X](iqx(HNil))
    })(this, ev)

  def asSymmetric: SLensAlg.Aux[P, Q, Q, A, A] =
    SLensAlg(hom, hom)(this, ev, ev)

  trait NatLensAlgLaw extends LensAlgLaw {

    // Monad homomorphism laws subsume natural transformation ones

    def hom1[A](a: A)(implicit eq: Equal[P[A]]): Boolean =
      hom(a.point[Q]) === a.point[P]

    def hom2[A, B](qa: Q[A])(f: A => Q[B])(implicit eq: Equal[P[B]]): Boolean =
      hom(qa >>= f) === (hom(qa) >>= (f andThen hom))
  }
}

object LensAlg {

  type Aux[P[_], Q2[_], A] = LensAlg[P, A] { type Q[x] = Q2[x] }

  private val fev1 = Functor[Id]

  def apply[P[_], Q2[_], A](
      hom2: Q2 ~> P)(implicit
      ev0: Monad[P],
      ev1: MonadState[Q2, A]): Aux[P, Q2, A] = new LensAlg[P, A] {
    type Q[x] = Q2[x]
    def point[X](x: => X) = ev0.point(x)
    def bind[X, Y](fx: P[X])(f: X => P[Y]): P[Y] = ev0.bind(fx)(f)
    implicit val ev = ev1
    implicit val fev = fev1
    val hom = hom2
  }
}
