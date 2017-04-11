package org.hablapps.phoropter
package core
package asymmetric
package raw
package indexed

import scalaz.{ Equal, Monad, MonadState }

trait IPrismAlg[P[_], I, A] extends Monad[P] {

  def getOption: P[Option[(I, A)]]

  def set(a: A): P[Unit]
}
