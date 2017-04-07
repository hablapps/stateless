package org.hablapps.phoropter
package core
package asymmetric
package raw

import scalaz.Monad

trait PrismAlg[P[_], A] extends Monad[P] {

  def getOption: P[Option[A]]

  def reverseGet(a: A): P[Unit]

  /* derived methods */

  def modify(f: A => A): P[Unit] = map(modifyOption(f))(_ => ())

  def modifyOption(f: A => A): P[Option[Unit]] =
    bind(getOption)(_.fold(point(Option.empty[Unit])) { a =>
      map(reverseGet(a))(Option.apply)
    })

  def isEmpty: P[Boolean] = map(getOption)(_.isEmpty)

  def nonEmpty: P[Boolean] = map(getOption)(_.nonEmpty)

  def find(p: A => Boolean): P[Boolean] = map(getOption)(_.fold(false)(p))

  def exist(p: A => Boolean): P[Boolean] = map(getOption)(_.fold(false)(p))

  def all(p: A => Boolean): P[Boolean] = map(getOption)(_.fold(true)(p))

}
