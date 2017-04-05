package org.hablapps.phoropter
package core
package symmetric

import scalaz.{ Monad, MonadState, ~> }

trait MonadSLens[P[_], L[_], R[_], A, B] extends Monad[P] {

  implicit val MSL: MonadState[L, A]
  implicit val MSR: MonadState[R, B]
  val homL: L ~> P
  val homR: R ~> P

  /* derived algebra */

  def getL: P[A] = homL(MSL.get)
  def setL(a: A): P[Unit] = homL(MSL.put(a))
  def getR: P[B] = homR(MSR.get)
  def setR(b: B): P[Unit] = homR(MSR.put(b))
}
