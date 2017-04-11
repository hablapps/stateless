package org.hablapps.stateless
package core
package asymmetric
package raw

import scalaz.Monad

trait IFoldAlg[P[_], I, A] extends Monad[P] {

  def getList: P[List[(I, A)]]
}
