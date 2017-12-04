package org.hablapps.stateless
package test

trait DepartmentAlg[Dp] {
  type F[_]
  type Pr; val Person: PersonAlg[Pr]

  def init(dep: SDepartment): F[Dp]
}

object DepartmentAlg {
  type Aux[Dp, Pr2, Ad2] = DepartmentAlg[Dp] {
    type Pr = Pr2
    val Person: PersonAlg.Aux[Pr, Ad2]
  }
}

trait PersonAlg[Pr] {
  type F[_]
  type Ad; val Address: AddressAlg[Ad]

  def init(per: SPerson): F[Pr]
}

object PersonAlg {
  type Aux[Pr, Ad2] = PersonAlg[Pr] { type Ad = Ad2 }
}

trait AddressAlg[Ad] {
  type F[_]

  def init(add: SAddress): F[Ad]
}
