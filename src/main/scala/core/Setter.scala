package org.hablapps.phoropter
package core

trait Setter[P[_], Q[_], A] {

  val M: Modifiable[Q, A]

  def modifyF[O](qo: Q[O]): P[O]

  // derived methods

  def modify(f: A => A): P[Unit] = modifyF(M.modify(f))

  def set(a: A): P[Unit] = modifyF(M.put(a))
}
