package org.hablapps.stateless
package core
package raw

import scalaz.Monad

trait IOptionalAlg[P[_], I, A] extends Monad[P] {

  def getOption: P[Option[(I, A)]]

  def setOption(a: A): P[Option[Unit]]

  /* derived methods */

  def modifyOption(f: A => A): P[Option[Unit]] =
    bind(getOption)(_.map(_._2).fold(point(Option.empty[Unit]))(setOption))

  def set(a: A): P[Unit] = void(setOption(a))

  def modify(f: A => A): P[Unit] = void(modifyOption(f))

  def isEmpty: P[Boolean] = map(getOption)(_.isEmpty)

  def nonEmpty: P[Boolean] = map(getOption)(_.nonEmpty)

  def find(p: ((I, A)) => Boolean): P[Option[(I, A)]] = map(getOption)(_.find(p))

  def exist(p: ((I, A)) => Boolean): P[Boolean] = map(getOption)(_.exists(p))

  def all(p: ((I, A)) => Boolean): P[Boolean] = map(getOption)(_.fold(true)(p))
}
