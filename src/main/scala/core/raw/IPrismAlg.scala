package org.hablapps.stateless
package core
package raw

import scalaz.Monad

trait IPrismAlg[P[_], I, A] extends Monad[P] {

  def getOption: P[Option[(I, A)]]

  def set(a: A): P[Unit]
}
