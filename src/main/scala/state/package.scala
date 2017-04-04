package org.hablapps.phoropter

import scalaz.{ MonadReader, MonadState, Reader, State }

package object `state` {

  def stateMonad[A]: MonadState[State[A, ?], A] = implicitly

  def readerMonad[A]: MonadReader[Reader[A, ?], A] = implicitly
}
