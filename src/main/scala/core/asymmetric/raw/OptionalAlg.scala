package org.hablapps.phoropter
package core
package asymmetric
package raw

import scalaz.Monad

trait OptionalAlg[P[_], A] extends Monad[P] {

  def getOption: P[Option[A]]

  def modifyOption(f: A => A): P[Option[Unit]]

  /* derived methods */

  def setOption(a: A): P[Option[Unit]] = modifyOption(_ => a)

  def set(a: A): P[Unit] = map(setOption(a))(_ => ())

  def modify(f: A => A): P[Unit] = map(modifyOption(f))(_ => ())

  def isEmpty: P[Boolean] = map(getOption)(_.isEmpty)

  def nonEmpty: P[Boolean] = map(getOption)(_.nonEmpty)

  def find(p: A => Boolean): P[Boolean] = map(getOption)(_.fold(false)(p))

  def exist(p: A => Boolean): P[Boolean] = map(getOption)(_.fold(false)(p))

  def all(p: A => Boolean): P[Boolean] = map(getOption)(_.fold(true)(p))
}
