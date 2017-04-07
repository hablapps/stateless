package org.hablapps.phoropter
package core
package symmetric

import scalaz.{ Monad, MonadState, ~> }
import scalaz.Id.Id

trait SLensAlg[P[_], L[_], R[_], A, B] extends SOpticAlg[P, L, R, A, B, MonadState, Id] {

  def getL: P[A] = homL[A](evL.get)

  def setL(a: A): P[Unit] = homL(evL.put(a))

  def getR: P[B] = homR[B](evR.get)

  def setR(b: B): P[Unit] = homR(evR.put(b))
}
