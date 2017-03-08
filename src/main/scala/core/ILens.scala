package org.hablapps.phoropter
package core

import scalaz.Kleisli

trait ILens[I, P[_], Q[_], A] {

  def get: P[(I, A)]

  def modify[O](qo: Q[O]): P[O]

  def set(a: A)(implicit K: Kleisli[Q, A, Unit]): P[Unit] = modify(K(a))
}
