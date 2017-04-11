package org.hablapps.stateless
package core
package raw

import scalaz.{ Equal, Monad }
import scalaz.syntax.monad._
import scalaz.syntax.equal._
import scalaz.std.option._

trait OptionalAlg[P[_], A] extends Monad[P] { self =>

  def getOption: P[Option[A]]

  def setOption(a: A): P[Option[Unit]]

  /* derived methods */

  def modifyOption(f: A => A): P[Option[Unit]] =
    bind(getOption)(_.fold(point(Option.empty[Unit]))(setOption))

  def set(a: A): P[Unit] = map(setOption(a))(_ => ())

  def modify(f: A => A): P[Unit] = map(modifyOption(f))(_ => ())

  def isEmpty: P[Boolean] = map(getOption)(_.isEmpty)

  def nonEmpty: P[Boolean] = map(getOption)(_.nonEmpty)

  def find(p: A => Boolean): P[Boolean] = map(getOption)(_.fold(false)(p))

  def exist(p: A => Boolean): P[Boolean] = map(getOption)(_.fold(false)(p))

  def all(p: A => Boolean): P[Boolean] = map(getOption)(_.fold(true)(p))

  trait OptionalAlgLaw {
    implicit val _: Monad[P] = self

    def getGet(implicit eq: Equal[P[(Option[A], Option[A])]]): Boolean =
      (getOption >>= (oa1 => getOption >>= (oa2 => (oa1, oa2).point[P]))) ===
        (getOption >>= (oa => (oa, oa).point[P]))

    def getPut(implicit eq: Equal[P[Option[Unit]]]): Boolean =
      (getOption >>= (_.fold(Option.empty[Unit].point[P])(setOption))) ===
        (getOption >>= (_.as(()).point[P]))

    def putGet(a: A)(implicit eq: Equal[P[Option[A]]]): Boolean =
      (setOption(a) >> getOption) === (setOption(a).map(_.as(a)))

    def putPut(a1: A, a2: A)(implicit eq: Equal[P[Option[Unit]]]): Boolean =
      (setOption(a1) >> setOption(a2)) === setOption(a2)
  }
}
