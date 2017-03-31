package org.hablapps.phoropter
package sandbox

import scalaz._, Scalaz._

object LawfulOptics {

  trait Lens[P[_], Q[_], A] extends Monad[P] {

    val MS: MonadState[Q, A]
    val lift: Q ~> P

    /* derived algebra */

    def get: P[A] = lift(MS.get)
    def set(a: A): P[Unit] = lift(MS.put(a))

    /* laws */

    implicit val _: Monad[P] = this

    def GG = (get >>= (a1 => get >>= (a2 => (a1, a2).point[P]))) <===>
             (get >>= (a => (a, a).point[P]))
    def GP = (get >>= set) <===> ().point[P]
    def PG = set(a1) >> get <===> (set(a1) >> a1.point[P])
    def PP = set(a1) >> set(a2) <===> set(a2)
  }

  trait Prism[P[_], Q[_], A] extends Monad[P] {

    implicit val MS: MonadState[Q, A]
    val lift: Q ~> λ[x => P[Option[x]]]

    /* derived algebra */

    def getOpt: P[Option[A]] = lift(MS.get)
    def set(a: A): P[Unit] = lift(MS.put(a)).map(_.get) // safe by laws

    /* laws */

    implicit val _: Monad[P] = this

    def GG = (getOpt >>= (oa1 => getOpt >>= (oa2 => (oa1, oa2).point[P]))) <===>
             (getOpt >>= (oa => (oa, oa).point[P]))
    def GP = (getOpt >>= (_.fold(().point[P])(set))) <===> ().point[P]
    def PG = set(a1) >> getOpt <===> (set(a1) >> a1.some.point[P])
    def PP = set(a1) >> set(a2) <===> set(a2)
  }

  trait Optional[P[_], Q[_], A] extends Monad[P] {

    implicit val MS: MonadState[Q, A]
    val lift: Q ~> λ[x => P[Option[x]]]

    /* derived algebra */

    def getOpt: P[Option[A]] = lift(MS.get)
    def set(a: A): P[Option[Unit]] = lift(MS.put(a))

    /* laws */

    implicit val _: Monad[P] = this

    def GG = (getOpt >>= (oa1 => getOpt >>= (oa2 => (oa1, oa2).point[P]))) <===>
             (getOpt >>= (oa => (oa, oa).point[P]))
    def GP = (getOpt >>= (_.fold(Option.empty[Unit].point[P])(set))) <===>
             (getOpt >>= (_.as(()).point[P]))
    def PG = set(a1) >> getOpt <===> (set(a1) >> getOpt.map(_.as(a1)))
    def PP = set(a1) >> set(a2) <===> set(a2)
  }

  trait Traversal[P[_], Q[_], A] extends Monad[P] {

    implicit val MS: MonadState[Q, A]
    val lift: Q ~> λ[x => P[List[x]]]

    /* derived algebra */

    def getAll: P[List[A]] = lift(MS.get)
    // XXX: `setAll` can't be implemented in terms of `lift`. On the other hand,
    // it's not even contained in the `Monocle` api...
    // def setAll(as: List[A]): P[Unit] = ???
    def set(a: A): P[Unit] = lift(MS.put(a)).as(())
    def modify(f: A => A): P[Unit] = lift(MS.modify(f)).as(())

    /* laws */

    implicit val _: Monad[P] = this

    def GG = (getAll >>= (as1 => getAll >>= (as2 => (as1, as2).point[P]))) <===>
             (getAll >>= (as => (as, as).point[P]))
    def MG = modify(f) >> getAll <===> getAll.map(_.map(f))
    def PP = set(a1) >> set(a2) <===> set(a2)
    ???
  }

  trait Getter[P[_], Q[_], A] extends Monad[P] {

    implicit val MR: MonadReader[Q, A]
    val lift: Q ~> P

    /* derived algebra */

    def get: P[A] = lift(MR.ask)

    /* laws */

    implicit val _: Monad[P] = this

    def GG = (get >>= (a1 => get >>= (a2 => (a1, a2).point[P]))) <===>
             (get >>= (a => (a, a).point[P]))
  }

  trait Fold[P[_], Q[_], A] extends Monad[P] {

    implicit val MR: MonadReader[Q, A]
    val lift: Q ~> λ[x => P[List[x]]]

    /* derived algebra */

    def getAll: P[List[A]] = lift(MR.ask)

    /* laws */

    implicit val _: Monad[P] = this

    def GG = (getAll >>= (as1 => getAll >>= (as2 => (as1, as2).point[P]))) <===>
             (getAll >>= (as => (as, as).point[P]))
  }

  trait Setter[P[_], Q[_], A] extends Monad[P] {

    implicit val MS: MonadState[Q, A]
    val lift: Q ~> P

    /* derived algebra */

    def modify(f: A => A): P[Unit] = lift(MS.modify(f))
    def set(a: A): P[Unit] = modify(_ => a)

    /* laws */

    implicit val _: Monad[P] = this

    def PP = set(a1) >> set(a2) <===> set(a2)
  }

  // XXX: our optics are p-biased, so `Iso` is just a `Lens` in disguise: same
  // algebra, same laws. This is weird!
  type Iso[P[_], Q[_], A] = Lens[P, Q, A]


  /* Symmetric Optics */

  trait SymLens[P[_], L[_], R[_], A, B] extends Monad[P] {

    val MSL: MonadState[L, A]
    val MSR: MonadState[R, B]
    val liftL: L ~> P
    val liftR: R ~> P

    /* derived algebra */

    def getL: P[A] = liftL(MSL.get)
    def setL(a: A): P[Unit] = liftL(MSL.put(a))
    def getR: P[B] = liftR(MSR.get)
    def setR(b: B): P[Unit] = liftR(MSR.put(b))

    /* laws */

    // TODO: entangled state monads paper
  }

  /* Indexed Optics */

  /* Law Syntax Stuff */

  // just a workaround to dulcify laws
  implicit class IsEqAux[F[_], A](fa: F[A]) {
    def <===>(other: F[A]): Boolean = true
  }

  // dummy values to feed laws
  def a1[A]: A = ???
  def a2[A]: A = ???
  def f[A]: A => A = ???
}
