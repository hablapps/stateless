package org.hablapps.phoropter
package core

import scalaz.Functor

trait Lens[P[_], Q[_], A] { self =>

  val G: Getter[Q, A]
  val M: Modifiable[Q, A]
  implicit val F: Functor[Q]

  def modifyF[O](qo: Q[O]): P[O]

  // derived methods

  def get: P[A] = modifyF(G.get)

  def gets[B](f: A => B): P[B] = modifyF(G.gets(f))

  def set(a: A): P[Unit] = modifyF(M.put(a))

  def modify(f: A => A): P[Unit] = modifyF(M.modify(f))

  def compose[R[_], B](other: Lens[Q, R, B]): Lens[P, R, B] =
    new Lens[P, R, B] {
      val G = other.G
      val M = other.M
      implicit val F = other.F
      def modifyF[O](ro: R[O]) = self.modifyF(other.modifyF(ro))
    }

  def compose[R[_], B](other: Traversal[Q, R, B]): Traversal[P, R, B] =
    new Traversal[P, R, B] {
      val G = other.G
      val M = other.M
      implicit val F = other.F
      def modifyF[O](ro: R[O]): P[List[O]] = self.modifyF(other.modifyF(ro))
    }
}
