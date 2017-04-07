package org.hablapps.phoropter
package core
package asymmetric
package raw

import scalaz.MonadState

trait LensAlg[P[_], A] extends MonadState[P, A] {

  def set(a: A): P[Unit] = put(a)

  def init: P[A] = get
}
