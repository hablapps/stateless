package org.hablapps.phoropter
package core
package asymmetric
package raw
package indexed

import scalaz.Monad

trait IFoldAlg[P[_], I, A] extends Monad[P] {

  def getList: P[List[(I, A)]]
}
