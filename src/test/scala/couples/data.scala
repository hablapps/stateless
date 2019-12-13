package org.hablapps.stateless
package test
package couples

import scalaz._, Scalaz._
import monocle._
import monocle.function.all._

import core.nat._, op.At, lib.MapAlg
import smonocle.nat.all._

/* data */

trait Couples[CS] {
  type P[_]
  type C

  val couple: Couple[C]

  val couples: FoldAlg.Aux[P, couple.P, C]
}

object Couples {
  import FoldAlg.filtered

  def under50[CS](
      cs: Couples[CS])(implicit
      MR: MonadReader[cs.couple.person.P, String]): cs.P[List[String]] = {
    import cs.couple._, person._

    cs.couples
      .composeGetter(her)
      .composeFold(filtered(age.get.map(_ < 50)))
      .composeGetter(name)
      .getList
  }
}

trait Couple[C] {
  type P[_]
  type Pr

  val person: Person[Pr]

  val her: GetterAlg.Aux[P, person.P, Pr]
  val him: GetterAlg.Aux[P, person.P, Pr]
}

trait Person[P] {
  type P[_]

  val name: GetterAlg[P, String]
  val age: GetterAlg[P, Int]
}

/* state */

object State {

  type SCouples = List[SCouple]
  case class SCouple(her: SPerson, him: SPerson)
  case class SPerson(name: String, age: Int)

  /* concrete optics */

  val couplesFl: Fold[SCouples, SCouple] = new Fold[SCouples, SCouple] {
    def foldMap[M: Monoid](f: SCouple => M)(s: SCouples): M = s.foldMap(f)
  }

  val herGt: monocle.Getter[SCouple, SPerson] = Getter(_.her)
  val himGt: monocle.Getter[SCouple, SPerson] = Getter(_.him)
  val nameGt: monocle.Getter[SPerson, String] = Getter(_.name)
  val ageGt: monocle.Getter[SPerson, Int] = Getter(_.age)

  /* data instance */

  val personR = new Person[SPerson] {
    type P[x] = Reader[SPerson, x]

    val name = fromGetter(nameGt)
    val age = fromGetter(ageGt)
  }

  val coupleR = new Couple[SCouple] {
    type P[x] = Reader[SCouple, x]
    type Pr = SPerson

    val person = personR

    val her = fromGetter(herGt)
    val him = fromGetter(himGt)
  }

  implicit val couplesR = new Couples[SCouples] {
    type P[x] = Reader[SCouples, x]
    type C = SCouple

    val couple = coupleR

    val couples = fromFold(couplesFl)
  }

  implicit val MR = new MonadReader[couplesR.couple.person.P, String] {

    def point[A](a: => A): couplesR.couple.person.P[A] = 
      Monad[Reader[SPerson, ?]].point(a)

    def bind[A, B](
        fa: couplesR.couple.person.P[A])(
        f: A => couplesR.couple.person.P[B]): couplesR.couple.person.P[B] =
      Monad[Reader[SPerson, ?]].bind(fa)(f)

    def ask: couplesR.couple.person.P[String] = Reader(_.name)

    def local[A](
      f: String => String)(
      fa: couplesR.couple.person.P[A]): couplesR.couple.person.P[A] = fa
  }

  def res: List[String] = Couples.under50(couplesR).run(List(
    SCouple(SPerson("Alex", 60), SPerson("Bert", 55)),
    SCouple(SPerson("Cora", 33), SPerson("Drew", 31)),
    SCouple(SPerson("Edna", 21), SPerson("Fred", 60))))
}

