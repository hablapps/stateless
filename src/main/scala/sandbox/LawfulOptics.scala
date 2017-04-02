package org.hablapps.phoropter
package sandbox

import scalaz._, Scalaz._

object LawfulOptics {

  trait Lens[P[_], Q[_], A] extends Monad[P] {

    implicit val MS: MonadState[Q, A]
    val hom: Q ~> P // monad homomorphism

    /* derived algebra */

    def _get: P[A] = hom(MS.get)
    def _set(a: A): P[Unit] = hom(MS.put(a))

    /* laws */

    import MS.{ get , put }
    implicit val _: Monad[P] = this

    def GG = hom(get >>= (a1 => get >>= (a2 => (a1, a2).point[Q]))) <===>
             hom(get >>= (a => (a, a).point[Q]))
    def GP = hom(get >>= put) <===> ().point[P]
    def PG = hom(put(a1) >> get) <===> (hom(put(a1)) >> a1.point[P])
    def PP = hom(put(a1) >> put(a2)) <===> hom(put(a2))
  }

  trait Prism[P[_], Q[_], A] extends Monad[P] {

    implicit val MS: MonadState[Q, A]
    val hom: Q ~> λ[x => P[Option[x]]]

    /* derived algebra */

    def getOpt: P[Option[A]] = hom(MS.get)
    def set(a: A): P[Unit] = hom(MS.put(a)).map(_.get) // safe by laws

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
    val hom: Q ~> λ[x => P[Option[x]]]

    /* derived algebra */

    def getOpt: P[Option[A]] = hom(MS.get)
    def set(a: A): P[Option[Unit]] = hom(MS.put(a))

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
    val hom: Q ~> λ[x => P[List[x]]]

    /* derived algebra */

    def getAll: P[List[A]] = hom(MS.get)
    // XXX: `setAll` can't be implemented in terms of `hom`. On the other hand,
    // it's not even contained in the `Monocle` api...
    // def setAll(as: List[A]): P[Unit] = ???
    def set(a: A): P[Unit] = hom(MS.put(a)).as(())
    def modify(f: A => A): P[Unit] = hom(MS.modify(f)).as(())

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
    val hom: Q ~> P

    /* derived algebra */

    def get: P[A] = hom(MR.ask)

    /* laws */

    implicit val _: Monad[P] = this

    def GG = (get >>= (a1 => get >>= (a2 => (a1, a2).point[P]))) <===>
             (get >>= (a => (a, a).point[P]))
  }

  trait Fold[P[_], Q[_], A] extends Monad[P] {

    implicit val MR: MonadReader[Q, A]
    val hom: Q ~> λ[x => P[List[x]]]

    /* derived algebra */

    def getAll: P[List[A]] = hom(MR.ask)

    /* laws */

    implicit val _: Monad[P] = this

    def GG = (getAll >>= (as1 => getAll >>= (as2 => (as1, as2).point[P]))) <===>
             (getAll >>= (as => (as, as).point[P]))
  }

  trait Setter[P[_], Q[_], A] extends Monad[P] {

    implicit val MS: MonadState[Q, A]
    val hom: Q ~> P

    /* derived algebra */

    def modify(f: A => A): P[Unit] = hom(MS.modify(f))
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
    val homL: L ~> P
    val homR: R ~> P

    /* derived algebra */

    def getL: P[A] = homL(MSL.get)
    def setL(a: A): P[Unit] = homL(MSL.put(a))
    def getR: P[B] = homR(MSR.get)
    def setR(b: B): P[Unit] = homR(MSR.put(b))

    /* laws */

    // TODO: entangled state monads paper
  }

  /* TODO: Indexed Optics & Polymorphic Optics */

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
