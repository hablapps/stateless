package org.hablapps.stateless
package core
package nat

import scalaz.{ Const, Equal, Functor, Monad, MonadState, State, ~> }
import scalaz.Id.Id
import scalaz.syntax.std.option._
import scalaz.syntax.monad._
import scalaz.syntax.equal._
import io.circe.{Json, Encoder, Decoder, DecodingFailure}

import shapeless.HNil

trait LensAlg[P[_], A] extends OpticAlg[P, A, MonadState, Id]
    with raw.LensAlg[P, A] {

  implicit val M: Monad[P]

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
    LensAlg(hom compose ln.hom)(M, ln.ev)

  def parLens(ln: LensAlg.Aux[P, Q, A]): TraversalAlg.Aux[P, Q, A] =
    TraversalAlg[P, Q, A](
      λ[Q ~> λ[x => P[List[x]]]] { qx =>
        hom(qx) >>= { a1 => ln.hom(qx) map { a2 => List(a1, a2) } }
      })(M, ev)

  /* transforming algebras */

  def asGetter: GetterAlg.Aux[P, Q, A] = GetterAlg(hom)(M, ev)

  def asOptional: OptionalAlg.Aux[P, Q, A] =
    OptionalAlg(λ[Q ~> λ[x => P[Option[x]]]](qx => hom(qx) map { _.some }))(M, ev)

  def asFold: FoldAlg.Aux[P, Q, A] = asGetter.asFold

  def asTraversal: TraversalAlg.Aux[P, Q, A] = asOptional.asTraversal

  def asSetter: SetterAlg.Aux[P, Q, A] = asTraversal.asSetter

  def asIndexed: ILensAlg.Aux[P, Q, HNil, A] =
    ILensAlg(new (λ[x => HNil => Q[x]] ~> P) {
      def apply[X](iqx: HNil => Q[X]): P[X] = hom[X](iqx(HNil))
    })(M, ev)

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
  sealed abstract class ADT[Out] { type Q[_] ; type F }
  case class Get[Q2[_], F2]() extends ADT[F2] { type Q[X] = Q2[X] ; type F = F2 }
  case class Put[Q2[_], F2](a: F2) extends ADT[Unit] { type Q[X] = Q2[X] ; type F = F2 }
  case class Hom[Q2[_], F2, A](qa: Q2[A]) extends ADT[A] { type Q[X] = Q2[X] ; type F = F2 }

  object ADT {
    type Aux[Q2[_], F2, O] = ADT[O] { type Q[X] = Q2[X] ; type F = F2 }
  }

  implicit def lensCirceSerializer[Q[_], A: Encoder: Decoder] = new CirceSerializer[ADT.Aux[Q, A, ?]] {

    def toJSON[X](adt: ADT.Aux[Q, A, X]): Json = adt match {
      case Put(a: A @unchecked) => // TODO(jfuentes): fu***** Scala...
        Json.obj(
          "name" -> Json.fromString("Put"),
          "a" -> Encoder[A].apply(a))
      case Get() =>
        Json.obj(
          "name" -> Json.fromString("Get"))
      case _ =>
        Json.Null
        // s"Can not serialize $adt"
        // throw new IllegalArgumentException(s"Can not serialize $adt")
    }
    def fromJSON(json: Json): ADT.Aux[Q, A, _] =
      (for {
        name <- json.hcursor.downField("name").as[String]
        res <-  name match {
                  case "Put" =>
                    for {
                      a1 <- json.hcursor.downField("a").as[Json]
                      a2 <- Decoder[A].apply(a1.hcursor)
                    } yield Put[Q, A](a2)
                  case "Get" => Right[DecodingFailure, ADT.Aux[Q, A, _]](Get[Q, A]())
                  case _ => ???
                }
      } yield res).getOrElse(???)

  }

  implicit val lensCQRS = new CQRS[ADT] {

    def kind[X](adt: ADT[X]): CQRS.Kind = adt match {
      case Put(_) => CQRS.Command
      case _ => CQRS.Query
    }
  }

  implicit def lensIso[Q2[_]: MonadState[?[_], A], A]: Iso.Aux[LensAlg.Aux[?[_], Q2, A], ADT.Aux[Q2, A, ?], Monad] =
    new IsoClass[Q2, A]

  class IsoClass[Q2[_]: MonadState[?[_], A], A] extends Iso[LensAlg.Aux[?[_], Q2, A]] {
    type ADT[X] = LensAlg.ADT[X] { type Q[X] = Q2[X] ; type F = A }
    type Ev[P[_]] = Monad[P]

    def to[P[_]](fp: LensAlg.Aux[P, Q2, A]): ADT ~> P =
      new (ADT ~> P) {
        def apply[X](adtX: ADT[X]): P[X] = adtX match {
          case _: Get[Q2, A] => fp.get
          case Put(a) => fp.put(a)
          case h: Hom[Q2, A, X] => fp.hom[X](h.qa) // ADT[P, Q, F, A] // MonadState[Q, A] evidence
        }
      }
    def from[P[_]: Monad](gp: ADT ~> P): LensAlg.Aux[P, Q2, A] =
      new LensAlg[P, A] {
        type Q[X] = Q2[X]

        val M = Monad[P]

        override def get: P[A] = gp(Get[Q, A]())
        override def put(a: A): P[Unit] = gp(Put[Q, A](a))

        val ev: MonadState[Q, A] = MonadState[Q, A]
        val hom: Q ~> P = new (Q ~> P) {
          def apply[X](qx: Q[X]) = gp(Hom[Q, A, X](qx))
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
    val M = ev0
    implicit val ev = ev1
    val hom = hom2
  }

  def state[S, A](get: S => A, put: A => S => S) = apply[State[S, ?], State[A, ?], A] {
    λ[State[A, ?] ~> State[S, ?]] { sa =>
      State { s =>
        val (a, o) = sa.run(get(s))
        (put(a)(s), o)
      }
    }
  }

}
