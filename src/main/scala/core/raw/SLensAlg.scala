package org.hablapps.stateless
package core
package raw

import scalaz.Monad

trait SLensAlg[P[_], A, B] extends Monad[P] {

  def getL: P[A]

  def setL(a: A): P[Unit]

  def getR: P[B]

  def setR(b: B): P[Unit]
}
