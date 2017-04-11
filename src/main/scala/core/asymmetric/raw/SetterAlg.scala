package org.hablapps.stateless
package core
package asymmetric
package raw

import scalaz.{ Equal, Monad }
import scalaz.syntax.equal._
import scalaz.syntax.monad._

trait SetterAlg[P[_], A] extends Monad[P] { self =>

  def modify(f: A => A): P[Unit]

  def set(a: A): P[Unit] = modify(_ => a)

  trait SetterAlgLaw {
    implicit val _: Monad[P] = self

    def putPut(a1: A, a2: A)(implicit eq: Equal[P[Unit]]): Boolean =
      (set(a1) >> set(a2)) === set(a2)
  }
}
