package org.hablapps.phoropter
package core
package asymmetric
package raw

import scalaz.Monad

trait SetterAlg[P[_], A] extends Monad[P] {

  def modify(f: A => A): P[Unit]

  def set(a: A): P[Unit] = modify(_ => a)
}
