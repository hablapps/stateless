package org.hablapps.stateless
package core
package raw

import scalaz.Monad

trait ISetterAlg[P[_], I, A] extends Monad[P] {

  def modify(f: A => A): P[Unit]

  /* derived methods */

  def set(a: A): P[Unit] = modify(_ => a)
}
