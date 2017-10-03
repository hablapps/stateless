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

  val fev: Functor[Id] = Functor[Id]

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

  /* laws */

  // Notice that monad homomorphism laws subsume natural transformation ones
  trait NatLensAlgLaw extends LensAlgLaw {

    def hom1[X](x: X)(implicit eq: Equal[P[X]]): Boolean =
      hom(x.point[Q]) === x.point[P]

    def hom2[X, Y](qx: Q[X])(f: X => Q[Y])(implicit eq: Equal[P[Y]]): Boolean =
      hom(qx >>= f) === (hom(qx) >>= (f andThen hom))
  }

  def natLensAlgLaw = new NatLensAlgLaw {}
}

object LensAlg {

  /* BEGIN */
  sealed abstract class ADT[P[_], Out] { type Q[_] ; type F } // TODO(jfuentes): Hide Focus
  case class Get[P[_], Q2[_], F2]() extends ADT[P, F2] { type Q[X] = Q2[X] ; type F = F2 }
  case class Put[P[_], Q2[_], F2](a: F2) extends ADT[P, Unit] { type Q[X] = Q2[X] ; type F = F2 }
  case class Point[P[_], Q2[_], F2, A](a: A) extends ADT[P, A] { type Q[X] = Q2[X] ; type F = F2 } // Let F existential
  case class Bind[P[_], Q2[_], F2, A, B](pa: P[A], f: A => P[B]) extends ADT[P, B] { type Q[X] = Q2[X] ; type F = F2 } // Let F existential
  case class Hom[P[_], Q2[_], F2, A](qa: Q2[A]) extends ADT[P, A] { type Q[X] = Q2[X] ; type F = F2 } // MonadState[Q, A] evidence // Let F existential

  object ADT {
    type Aux[P[_], Q2[_], F2, O] = ADT[P, O] { type Q[X] = Q2[X] ; type F = F2 }
  }

  implicit def lensIso[Q2[_]: MonadState[?[_], A], A]: Iso.Aux[LensAlg.Aux[?[_], Q2, A], ADT.Aux[?[_], Q2, A, ?]] =
    new IsoClass[Q2, A]

  class IsoClass[Q2[_]: MonadState[?[_], A], A] extends Iso[LensAlg.Aux[?[_], Q2, A]] {
    type ADT[P[_], X] = LensAlg.ADT[P, X] { type Q[X] = Q2[X] ; type F = A }

    def mapHK[P[_], Q[_]](nat: P ~> Q) = new (ADT[P, ?] ~> ADT[Q, ?]) {
      def apply[X](px: ADT[P, X]): ADT[Q, X] = px match {
        case _: Get[P, Q2, A] => Get[Q, Q2, A]()
        case Put(a) => Put[Q, Q2, A](a)
        case Point(a) => Point[Q, Q2, A, X](a)
        case Bind(pa, f) =>
          def go[I](pi: P[I]) = Bind[Q, Q2, A, I, X](nat(pi), f andThen nat)
          go(pa)
        case Hom(qa) =>
          def go[I](qi: Q2[I]) = Hom[Q, Q2, A, I](qi)
          go(qa)
      }
    }

    def recover[P[_]: Monad](transf: λ[α=>(ADT[P, α], P[α])] ~> P) = λ[λ[α=>(ADT[P, α], P[α])] ~> P] { t => t._1 match {
      case b: Bind[P, Q2, A, _, _] => b.pa flatMap b.f
      case _ => transf(t)
    }}

    def kind[P[_], X](adt: ADT[P, X]): Iso.Kind = adt match {
      case Put(_) => Iso.Command
      case _ => Iso.Query
    }

    def to[P[_]](fp: LensAlg.Aux[P, Q2, A]): ADT[P, ?] ~> P =
      new (ADT[P, ?] ~> P) {
        def apply[X](adtX: ADT[P, X]): P[X] = adtX match {
          case _: Get[P, Q2, A] => fp.get
          case Put(a) => fp.put(a)
          case Point(a) => fp.point(a) // ADT[P, Q, F, A]
          case Bind(pa, f) => fp.bind(pa)(f) // ADT[P, Q, F, B]
          case h: Hom[P, Q2, A, X] => fp.hom[X](h.qa) // ADT[P, Q, F, A] // MonadState[Q, A] evidence
        }
      }
    def from[P[_]](gp: ADT[P, ?] ~> P): LensAlg.Aux[P, Q2, A] =
      new LensAlg[P, A] {
        type Q[X] = Q2[X]

        override def get: P[A] = gp(Get[P, Q, A]())
        override def put(a: A): P[Unit] = gp(Put[P, Q, A](a))

        def point[X](a: => X): P[X] = gp(Point[P, Q, A, X](a))
        def bind[X, Y](fa: P[X])(f: X => P[Y]): P[Y] = gp(Bind[P, Q, A, X, Y](fa, f))

        val ev: MonadState[Q, A] = MonadState[Q, A]
        val hom: Q ~> P = new (Q ~> P) {
          def apply[X](qx: Q[X]) = gp(Hom[P, Q, A, X](qx))
        }
      }
  }
  /* END */

  type Aux[P[_], Q2[_], A] = LensAlg[P, A] { type Q[x] = Q2[x] }

  def apply[P[_], Q2[_], A](
      hom2: Q2 ~> P)(implicit
      ev0: Monad[P],
      ev1: MonadState[Q2, A]): Aux[P, Q2, A] = new LensAlg[P, A] {
    type Q[x] = Q2[x]
    def point[X](x: => X) = ev0.point(x)
    def bind[X, Y](fx: P[X])(f: X => P[Y]): P[Y] = ev0.bind(fx)(f)
    implicit val ev = ev1
    val hom = hom2
  }

  import scalaz.{NaturalTransformation, StateT}
  def state[A] = apply[StateT[Id, A, ?], StateT[Id, A, ?], A](NaturalTransformation.refl[StateT[Id, A, ?]])

}
