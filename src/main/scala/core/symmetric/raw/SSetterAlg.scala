package org.hablapps.phoropter
package core
package symmetric
package raw

import scalaz.Monad

trait SSetterAlg[P[_], A, B] extends Monad[P] {

  def setL(a: A): P[Unit]

  def setR(b: B): P[Unit]
}
