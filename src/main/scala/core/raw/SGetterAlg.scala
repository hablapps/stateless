package org.hablapps.stateless
package core
package symmetric
package raw

import scalaz.Monad

trait SGetterAlg[P[_], A, B] extends Monad[P] {

  def getL: P[A]

  def getR: P[B]
}
