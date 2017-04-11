package org.hablapps.stateless
package core
package asymmetric
package raw
package indexed

import scalaz.Monad

trait ILensAlg[P[_], I, A] extends Monad[P] {

  def get: P[(I, A)]

  def set(a: A): P[Unit]
}
