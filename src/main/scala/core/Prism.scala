package org.hablapps.phoropter
package core

import scalaz.Functor
import scalaz.syntax.functor._

trait Prism[P[_], Q[_], A] {

  val G: Getter[Q, A]
  val M: Modifiable[Q, A]

  def modifyF[O](qo: Q[O]): P[Option[O]]

  // derived methods

  def getOption: P[Option[A]] = modifyF(G.get)

  def modifyOption(f: A => A): P[Option[Unit]] = modifyF(M.modify(f))

  def set(a: A)(implicit F: Functor[P]): P[Unit] = modifyF(M.put(a)).as(())

  def modify(f: A => A)(implicit F: Functor[P]): P[Unit] =
    modifyF(M.modify(f)).as(())
}
