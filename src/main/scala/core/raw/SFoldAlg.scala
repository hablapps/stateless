package org.hablapps.stateless
package core
package raw

import scalaz.Monad

trait SFoldAlg[P[_], A, B] extends Monad[P] {

  def getListL: P[List[A]]

  def getListR: P[List[B]]
}
