package org.hablapps.stateless
package core
package raw

import scalaz.{ Equal, Monad }
import scalaz.syntax.monad._
import scalaz.syntax.equal._
import scalaz.std.option._

trait OptionalAlg[P[_], A] {

  implicit val M: Monad[P]

  def getOption: P[Option[A]]

  def setOption(a: A): P[Option[Unit]]

  /* derived methods */

  def setOption2(a: A): P[Option[Unit]] =
    getOption >>= { oa =>
      oa.fold(M.point(()))(_ => set(a)) map (_ => oa.void)
    }

  def modifyOption(f: A => A): P[Option[Unit]] =
    getOption >>= (_.fold(M.point(Option.empty[Unit]))(setOption))

  def set(a: A): P[Unit] = setOption(a).void

  def modify(f: A => A): P[Unit] = modifyOption(f).void

  def isEmpty: P[Boolean] = getOption map (_.isEmpty)

  def nonEmpty: P[Boolean] = getOption map (_.nonEmpty)

  def find(p: A => Boolean): P[Option[A]] = getOption map (_.find(p))

  def exist(p: A => Boolean): P[Boolean] = getOption map (_.exists(p))

  def all(p: A => Boolean): P[Boolean] = getOption map (_.fold(true)(p))

  trait OptionalAlgLaw {

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

  def optionalAlgLaw = new OptionalAlgLaw {}
}
