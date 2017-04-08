package org.hablapps.phoropter
package core
package symmetric
package raw

import scalaz.Monad

trait STraversalAlg[P[_], A, B] extends Monad[P] {

  def getListL: P[List[A]]

  def modifyListL(f: A => A): P[List[Unit]]

  def getListR: P[List[B]]

  def modifyListR(f: B => B): P[List[Unit]]
}
