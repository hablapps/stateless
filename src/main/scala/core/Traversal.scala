package org.hablapps.phoropter
package core

import Function.const
import scalaz._, Scalaz._

trait Traversal[P[_], Q[_], A] {

  val G: Getter[Q, A]
  val M: Modifiable[Q, A]
  implicit val F: Functor[Q]

  def modifyF[O](qo: Q[O]): P[List[O]]

  // derived methods

  def getAll: P[List[A]] = modifyF(G.get)

  def getsAll[B](f: A => B): P[List[B]] = modifyF(G.gets(f))

  def set(a: A)(implicit F2: Functor[P]): P[Unit] = modifyF(M.put(a)).as(())

  def modify(f: A => A)(implicit F2: Functor[P]): P[Unit] =
    modifyF(M.modify(f)).as(())

  def count(implicit F2: Functor[P]): P[Int] =
    modifyF(G.gets(const(1))).map(_.sum)
}
