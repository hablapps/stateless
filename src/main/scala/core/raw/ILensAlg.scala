package org.hablapps.stateless
package core
package raw

import scalaz.Monad

trait ILensAlg[P[_], I, A] extends Monad[P] {

  def get: P[(I, A)]

  def set(a: A): P[Unit]
}
