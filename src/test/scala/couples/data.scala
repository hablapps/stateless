package org.hablapps.stateless
package test
package couples

import scalaz._, Scalaz._
import monocle.function.all._

import core.nat._, op.At, lib.MapAlg
import smonocle.nat.all._

trait Couples[CS] {
  type P[_]
  type C

  val couple: Couple[C]

  val couples: FoldAlg.Aux[P, couple.P, C]
}

object Couples {
  import FoldAlg.filtered

  def under50[CS](implicit 
      cs: Couples[CS],
      MR: MonadReader[cs.couple.person.P, String]): cs.P[List[String]] = {
    import cs._, couple._, person._

    couples.composeGetter(her)
           .composeFold(filtered(age.get.map(_ > 50)))
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

  val name: GetterField[P, String]
  val age: GetterField[P, Int]
}

