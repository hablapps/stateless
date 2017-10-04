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

  trait Iso[TC[_[_]]] {
    type ADT[_[_], _] // Also: type NAT[P[_]] = ADT[P, ?] ~> P

    def mapHK[P[_], Q[_]](nat: P ~> Q): ADT[P, ?] ~> ADT[Q, ?] // TODO(jfuentes) return TC[P] => TC[Q] instead?
    def recover[P[_]: Monad](transf: λ[α=>(ADT[P, α], P[α])] ~> P): λ[α=>(ADT[P, α], P[α])] ~> P

    def kind[P[_], A](adt: ADT[P, A]): Iso.Kind

    import io.circe.Json
    def toJSON[P[_], A](adt: ADT[P, A]): Json
    def fromJSON[P[_]](json: Json): ADT[P, _]

    def dimapHK[P[_], Q[_]: Monad](
        unlift: Q ~> P,
        lift: P ~> Q,
        transf: λ[α=>(ADT[Q, α], Q[α])] ~> Q)(orig: ADT[P, ?] ~> P): ADT[Q, ?] ~> Q =
      new (ADT[Q, ?] ~> Q) {
        def apply[A](adtQ: ADT[Q, A]): Q[A] = {
          mapHK(unlift)(adtQ) |> { (adtP: ADT[P, A]) =>
            orig(adtP) |> { (p: P[A]) =>
              recover(transf).apply(adtQ, lift(p))
            }
          }
          // (unlift andThen orig andThen lift)(adtQ)
        }
      }

    def to[P[_]](fp: TC[P]): ADT[P, ?] ~> P
    def from[P[_]](gp: ADT[P, ?] ~> P): TC[P]
  }

  object Iso {
    type Aux[TC[_[_]], ADT2[_[_], _]] = Iso[TC] { type ADT[P[_], X] = ADT2[P, X] }

    sealed abstract class Kind
    case object Query extends Kind
    case object Command extends Kind
  }

}
