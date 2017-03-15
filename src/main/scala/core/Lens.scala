package org.hablapps.phoropter
package core

import scalaz._, Scalaz._

trait Lens[P[_], Q[_], A] { self =>

  implicit val MS: MonadState[Q, A]

  def modifyF[O](qo: Q[O]): P[O]

  // derived methods

  def get: P[A] = modifyF[A](MS.get)

  def set(a: A): P[Unit] = modifyF[Unit](MS.put(a))

  def modify(f: A => A): P[Unit] = modifyF[Unit](MS.modify(f))

  def compose[R[_], B](other: Lens[Q, R, B]): Lens[P, R, B] =
    new Lens[P, R, B] {
      implicit val MS = other.MS
      def modifyF[O](ro: R[O]): P[O] = self.modifyF(other.modifyF(ro))
    }

  def compose[R[_], B](other: Traversal[Q, R, B]): Traversal[P, R, B] =
    new Traversal[P, R, B] {
      implicit val MS = other.MS
      def modifyF[O: Monoid](ro: R[O]): P[O] = self.modifyF(other.modifyF(ro))
    }
}
