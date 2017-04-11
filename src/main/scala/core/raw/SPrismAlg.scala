package org.hablapps.stateless
package core
package symmetric
package raw

import scalaz.Monad

trait SPrismAlg[P[_], A, B] extends Monad[P] {

  def getOptionL: P[Option[A]]

  def setL(a: A): P[Unit]

  def getOptionR: P[Option[B]]

  def setR(b: B): P[Unit]
}
