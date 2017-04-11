package org.hablapps.stateless
package core
package raw

import scalaz.Monad

trait SOptionalAlg[P[_], A, B] extends Monad[P] {

  def getOptionL: P[Option[A]]

  def setOptionL(a: A): P[Option[Unit]]

  def getOptionR: P[Option[B]]

  def setOptionR(b: B): P[Option[Unit]]
}
