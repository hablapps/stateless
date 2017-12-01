package org.hablapps.stateless
package test

import scalaz._, Scalaz._
import org.hablapps.puretest._

import ZipCodeSpecState._

class ZipCodeSpecState extends ZipCodeSpec.ScalaTest[Program](
    ZipCodeState.stateSystem[F],
    ZipCodeState.stateView[F],
    StateTester[Program,System,PuretestError[Throwable]]
      .apply(System(SDepartment(0,SPerson("theboss",None),List())))) {

  val Alg = new DepartmentAlg[Sys.Dp] {
    type F[X] = Id[X]
    type Pr = Sys.Department.Pr
    val Person = ???

    def init(dep: SDepartment): Id[SDepartment] = dep
  }

  implicit val M4: Monad[Alg.Person.F] = ???

  val Lift: Alg.Person.F ~> Program = new (Alg.Person.F ~> Program) {
    def apply[A](fa: Alg.Person.F[A]): Program[A] =
      // fa.liftM[StateT[?[_], System, ?]]
      ???
  }
}

object ZipCodeSpecState{
  type F[T]=Either[PuretestError[Throwable],T]
  type Program[T]=StateT[F,System,T]
}
