package org.hablapps.stateless

// import scalaz.{ Const, Monad, MonadState, MonadReader, StateT }
// import scalaz.syntax.monad._

import scalaz._, Scalaz._

package object `core` {

  // XXX: this has to be standard somewhere
  implicit def fromStateToReader[F[_], A](ms: MonadState[F, A]): MonadReader[F, A] =
    new MonadReader[F, A] {
      def point[X](x: => X) = ms.point(x)
      def bind[X, Y](fx: F[X])(f: X => F[Y]): F[Y] = ms.bind(fx)(f)
      def ask = ms.get
      def local[X](f: A => A)(fx: F[X]): F[X] = {
        implicit val ims: MonadState[F, A] = ms
        for {
          a <- ms.get
          _ <- ms.put(f(a))
          x <- fx
          _ <- ms.put(a)
        } yield x
      }
    }

  implicit def stateTMonadReader[F[_]: Monad, A]: MonadReader[StateT[F, A, ?], A] =
    fromStateToReader[StateT[F, A, ?], A](MonadState[StateT[F, A, ?], A])

  trait CirceSerializer[ADT[_]] {
    import io.circe.Json

    def toJSON[A](adt: ADT[A]): Json
    def fromJSON(json: Json): ADT[_]
  }

  trait CQRS[ADT[_]] {
    def kind[A](adt: ADT[A]): CQRS.Kind
  }

  object CQRS {
    sealed abstract class Kind
    case object Query extends Kind
    case object Command extends Kind
  }

  trait Iso[TC[_[_]]] {
    type ADT[_] // Also: type NAT[P[_]] = ADT[P, ?] ~> P
    type Ev[_[_]]

    def to[P[_]](fp: TC[P]): ADT ~> P
    def from[P[_]: Ev](gp: ADT ~> P): TC[P]

    /* DERIVED */
    def mapHK[P[_], Q[_]](nat: P ~> Q)(adtP: ADT ~> P): ADT ~> Q =
      λ[ADT ~> Q] { _ |> adtP |> nat }
  }

  object Iso {
    type Aux[TC[_[_]], ADT2[_], Ev2[_[_]]] = Iso[TC] { type ADT[X] = ADT2[X] ; type Ev[P[_]] = Ev2[P] }
    type WithEv[TC[_[_]], Ev2[_[_]]] = Iso[TC] { type Ev[P[_]] = Ev2[P] }
  }

}
