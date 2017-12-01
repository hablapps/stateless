package org.hablapps.stateless
package test

import scalaz.State

import core.nat._

// Data layer

trait SystemData[P[_]]{
  type Dp; val Department: Department[Dp]

  def create: P[Unit]
  def destroy: P[Unit]
  def init(dep: SDepartment): P[Unit]

  val department: LensAlg.Aux[P, Department.P, Dp]
}

object SystemData{
  type Aux[P1[_], Dp2] = SystemData[P1] { type Dp = Dp2 }
}

trait Department[Dp] {
  type P[_]
  type Pr; val Person: Person[Pr]

  val budget: LensAlg[P, Int]
  val head: LensAlg.Aux[P, Person.P, Pr]
  val members: TraversalAlg.Aux[P, Person.P, Pr]
}

object Department{
  type Aux[Dp,P1[_]] = Department[Dp]{ type P[t]=P1[t] }
}

trait Person[Pr] {
  type P[_]
  type Ad; val Address: Address[Ad]

  val name: LensAlg[P, String]
  val optAddress: OptionalAlg.Aux[P, Address.P, Ad]
}

object Person{
  type Aux[Pr,P1[_]] = Person[Pr]{ type P[t]=P1[t] }
}

trait Address[Ad] {
  type P[_]

  val city: LensAlg[P, String]
  val zip: LensAlg[P, Int]
}

object Address{
  type Aux[Ad,P1[_]] = Address[Ad]{ type P[t]=P1[t] }
}
