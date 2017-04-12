package org.hablapps.stateless
package core
package raw

import scalaz.{ Equal, Monad }
import scalaz.syntax.monad._
import scalaz.syntax.equal._
import scalaz.syntax.optional._
import scalaz.syntax.std.option._
import scalaz.std.option._

trait PrismAlg[P[_], A] extends Monad[P] { self =>

  def getOption: P[Option[A]]

  def set(a: A): P[Unit]

  /* derived methods */

  def modify(f: A => A): P[Unit] = void(modifyOption(f))

  def modifyOption(f: A => A): P[Option[Unit]] =
    bind(getOption)(_.fold(point(Option.empty[Unit])) { a =>
      map(set(a))(Option.apply)
    })

  def isEmpty: P[Boolean] = map(getOption)(_.isEmpty)

  def nonEmpty: P[Boolean] = map(getOption)(_.nonEmpty)

  def find(p: A => Boolean): P[Boolean] = map(getOption)(_.fold(false)(p))

  def exist(p: A => Boolean): P[Boolean] = map(getOption)(_.fold(false)(p))

  def all(p: A => Boolean): P[Boolean] = map(getOption)(_.fold(true)(p))

  trait PrismAlgLaw {
    implicit val _: Monad[P] = self

    def getGet(implicit eq: Equal[P[(Option[A], Option[A])]]): Boolean =
      (getOption >>= (oa1 => getOption >>= (oa2 => (oa1, oa2).point[P]))) ===
        (getOption >>= (oa => (oa, oa).point[P]))

    def getPut(implicit eq: Equal[P[Unit]]): Boolean =
      (getOption >>= (_.fold(().point[P])(set))) === ().point[P]

    def putGet(a: A)(implicit eq: Equal[P[Option[A]]]): Boolean =
      (set(a) >> getOption) === (set(a) >> a.some.point[P])

    def putPut(a1: A, a2: A)(implicit eq: Equal[P[Unit]]): Boolean =
      (set(a1) >> set(a2)) === set(a2)
  }
}
