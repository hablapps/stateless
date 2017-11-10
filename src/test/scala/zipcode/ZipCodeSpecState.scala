package org.hablapps.stateless
package test

import scalaz._, Scalaz._
import org.hablapps.puretest._

import ZipCodeSpecState._

class ZipCodeSpecState extends ZipCodeSpec.ScalaTest[Program](
  ZipCodeState.stateSystem[F],
  ZipCodeState.stateView[F],
  StateTester[Program,System,PuretestError[Throwable]]
    .apply(System(SDepartment(0,SPerson("theboss",None),List())))
)

object ZipCodeSpecState{
  type F[T]=Either[PuretestError[Throwable],T]
  type Program[T]=StateT[F,System,T]
}
