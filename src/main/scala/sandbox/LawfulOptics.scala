package org.hablapps.phoropter
package sandbox

import scalaz._, Scalaz._

object LawfulOptics {

  trait ILens[P[_], Q[_], I, A] extends Monad[P] {

    implicit val MS: MonadState[Q, (I, A)]
    val hom: Q ~> P

    def get: P[(I, A)] = hom(MS.get)
    def put(a: A): P[Unit] = bind(hom(MS.get)) {
      case (i, _) => hom(MS.put((i, a)))
    }
  }

  trait Lens[P[_], Q[_], A] extends MonadState[P, A] {

    implicit val MS: MonadState[Q, A]
    val hom: Q ~> P // monad homomorphism

    /* derived algebra */

    def get: P[A] = hom(MS.get)
    def put(a: A): P[Unit] = hom(MS.put(a))

    /* laws */

    import MS.{ get => qget, put => qput }
    implicit val _: Monad[P] = this

    // XXX: these laws are redundant, as long as `hom` is a monad homomorphism

    def GG = hom(qget >>= (a1 => qget >>= (a2 => (a1, a2).point[Q]))) <===>
             hom(qget >>= (a => (a, a).point[Q]))
    def GP = hom(qget >>= qput) <===> ().point[P]
    def PG = hom(qput(a1) >> qget) <===> (hom(qput(a1)) >> a1.point[P])
    def PP = hom(qput(a1) >> qput(a2)) <===> hom(qput(a2))
  }

  // MonadPrism
  trait Prism[P[_], Q[_], A] extends Monad[P] {

    implicit val MS: MonadState[Q, A]
    val hom: Q ~> λ[x => P[Option[x]]]
    // val hom: Q ~> OptionT[P, ?]

    /* derived algebra */

    def getOpt: P[Option[A]] = hom(MS.get)
    def set(a: A): P[Unit] = hom(MS.put(a)).map(_.get) // safe by laws

    def modify(f: A => A): P[Unit] =
      getOpt >>= (_.fold(().point[P])(a => set(f(a))))

    /* laws */

    import MS.{ get, put }
    implicit val _: Monad[P] = this

    // 1) preserves MonadState laws
    // XXX: do we really need them? aren't they redundant given MS laws?
    def PGG = hom(get >>= (a1 => get >>= (a2 => (a1, a2).point[Q]))) <===>
              hom(get >>= (a => (a, a).point[Q]))
    // def PXX = hom(MS.law_i.left) <===> hom(MS.law_i.right)

    // 2) introduces Prism laws
    def GG = (hom(get) >>= (oa1 => hom(get) >>= (oa2 => (oa1, oa2).point[P]))) <===>
             (hom(get) >>= (oa => (oa, oa).point[P]))
    def GP = (hom(get) >>= (_.fold(().some.point[P])(a => hom(put(a))))) <===>
             ().some.point[P]
    def PG = hom(put(a1)) >> hom(get) <===> (hom(put(a1)) >> a1.some.point[P])
    def PP = hom(put(a1)) >> hom(put(a2)) <===> hom(put(a2))

    // def GG = (getOpt >>= (oa1 => getOpt >>= (oa2 => (oa1, oa2).point[P]))) <===>
    //          (getOpt >>= (oa => (oa, oa).point[P]))
    // def GP = (getOpt >>= (_.fold(().point[P])(set))) <===> ().point[P]
    // def PG = set(a1) >> getOpt <===> (set(a1) >> a1.some.point[P])
    // def PP = set(a1) >> set(a2) <===> set(a2)
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
    // def setAll(xs: List[A]): P[Unit] = ???

    def set(a: A): P[Unit] = hom(MS.put(a)).as(())
    def modify(f: A => A): P[Unit] = hom(MS.modify(f)).as(())

    /* laws */

    implicit val _: Monad[P] = this

    def GG = (getAll >>= (as1 => getAll >>= (as2 => (as1, as2).point[P]))) <===>
             (getAll >>= (as => (as, as).point[P]))
    def MG = modify(f) >> getAll <===> getAll.map(_.map(f))
    def PP = set(a1) >> set(a2) <===> set(a2)
    // ???
  }

  object Traversal {
    import monocle.{ Traversal => MTraversal }

    // def stateInstance[S, A](
    //     mtr: MTraversal[S, A]) = new Traversal[State[S, ?], State[A, ?], A] {
    //   private val M = IndexedStateT.stateTMonadState[S, Id]
    //   def point[X](x: => X) = M.point(x)
    //   def bind[X, Y](st: State[S, X])(f: X => State[S, Y]) = M.bind(st)(f)
    //
    //   implicit val MS = IndexedStateT.stateTMonadState[A, Id]
    //   val hom = new (State[A, ?] ~> ListT[State[S, ?], ?]) {
    //     def apply[X](st: State[A, X]): ListT[State[S, ?], X] =
    //       ListT[State[S, ?], X](State(s => (
    //         mtr.modify(st.exec)(s),
    //         mtr.getAll(s).traverse(st.eval))))
    //   }
    // }
    //
    // case class Person(name: String, last: String, age: Int)
    //
    // val ageTr = new MTraversal[Person, String] {
    //   def modifyF[F[_]: Applicative](f: String => F[String])(s: Person): F[Person] =
    //     (f(s.name) |@| f(s.last)) { Person(_, _, s.age) }
    // }
  }

  trait Getter[P[_], Q[_], A] extends MonadReader[P, A] {

    implicit val MR: MonadReader[Q, A]
    val hom: Q ~> P // monad homomorphism

    /* derived algebra */

    def get: P[A] = hom(MR.ask)

    /* laws */

    import MR.{ ask => qask }
    implicit val _: Monad[P] = this

    // XXX: these laws are implicit, as long as `hom` is a monad homomorphism

    def GG = hom(qask >>= (a1 => qask >>= (a2 => (a1, a2).point[Q]))) <===>
             (hom(qask) >>= (a => (a, a).point[P]))
  }

  trait Fold[P[_], Q[_], A] extends Monad[P] {

    implicit val MR: MonadReader[Q, A]
    val hom: Q ~> ListT[P, ?] // monad homomorphism

    /* derived algebra */

    def getAll: P[List[A]] = hom(MR.ask).run

    /* laws */

    import MR.ask
    implicit val _: Monad[P] = this

    def GG = hom(ask >>= (a1 => ask >>= (a2 => (a1, a2).point[Q]))).run <===>
             hom(ask >>= (a => (a, a).point[Q])).run
  }

  trait Setter[P[_], Q[_], A] extends Monad[P] {

    implicit val MS: MonadState[Q, A]
    val hom: Q ~> P

    /* derived algebra */

    def modify(f: A => A): P[Unit] = hom(MS.modify(f))
    def set(a: A): P[Unit] = modify(_ => a)

    /* laws */

    import MS.put
    implicit val _: Monad[P] = this

    // XXX: these laws are implicit, as long as `hom` is a monad homomorphism

    def PP = hom(put(a1) >> put(a2)) <===> hom(put(a2))
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
