package org.hablapps.stateless
package core
package asymmetric
package raw

import scalaz.Monad

trait ISetterAlg[P[_], I, A] extends Monad[P] {

  def modify(f: A => A): P[Unit]

  def set(a: A): P[Unit] = modify(_ => a)
}
