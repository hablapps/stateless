package org.hablapps.phoropter
package core
package asymmetric
package raw

import scalaz.MonadReader

trait GetterAlg[P[_], A] extends MonadReader[P, A] {

  def get: P[A] = ask

  // FIXME: dummy implementation
  def local[X](f: A => A)(px: P[X]) = px
}
