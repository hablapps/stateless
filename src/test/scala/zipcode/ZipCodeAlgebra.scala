package org.hablapps.stateless
package test

trait DepartmentAlg[Dp] {
  type F[_]
  type Pr; val Person: PersonAlg[Pr]

  def init(dep: SDepartment): F[Dp]
}

object DepartmentAlg {
  type Aux[Dp, Pr2] = DepartmentAlg[Dp] { type Pr = Pr2 }
}

trait PersonAlg[Pr] {
  type F[_]

  def init(per: SPerson): F[Pr]
}
