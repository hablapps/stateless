package org.hablapps.stateless
package test

import scalaz._, Scalaz._
import org.hablapps.puretest._

import ZipCodeSpecState._

class ZipCodeSpecState
    extends scalatestImpl.FunSpec[Program, Throwable]
    with ZipCodeSpec[Program] {

  implicit val M = ZipCodeSpecState.M
  implicit val RE = ZipCodeSpecState.RE
  implicit val HE = ZipCodeSpecState.HE

  val Sys = ZipCodeState.stateSystem[F]
  val Ser = ZipCodeState.stateView[F]
  val Tester = StateTester[Program,System,PuretestError[Throwable]]
    .apply(System(SDepartment(0,SPerson("theboss",None),List())))

  val Alg = new DepartmentAlg[SDepartment] {
    type F[X] = Id[X]
    type Pr = SPerson
    val Person = new PersonAlg[SPerson] {
      type F[X] = Id[X]
      def init(per: SPerson) = per
    }
    def init(dep: SDepartment) = dep
  }

  val Lift: Alg.Person.F ~> Program = λ[Alg.Person.F ~> Program](M.point(_))
}

object ZipCodeSpecState {
  type F[T] = Either[PuretestError[Throwable], T]
  type Program[T]  = StateT[F, System, T]
  val M = Monad[Program]
  val RE = RaiseError[Program, PuretestError[Throwable]]
  val HE = HandleError[Program, Throwable]
}
