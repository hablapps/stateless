package org.hablapps.stateless
package core
package raw

import scalaz.{ Equal, Monad }
import scalaz.syntax.monad._
import scalaz.syntax.equal._
import scalaz.syntax.optional._
import scalaz.syntax.std.option._
import scalaz.std.option._

trait PrismAlg[P[_], A] { self =>

  implicit val M: Monad[P]

  def getOption: P[Option[A]]

  def set(a: A): P[Unit]

  /* derived methods */

  def modify(f: A => A): P[Unit] = modifyOption(f).void

  def modifyOption(f: A => A): P[Option[Unit]] =
    getOption >>= (_.fold(M.point(Option.empty[Unit])) { a =>
      set(a) map Option.apply
    })

  def isEmpty: P[Boolean] = getOption map (_.isEmpty)

  def nonEmpty: P[Boolean] = getOption map (_.nonEmpty)

  def find(p: A => Boolean): P[Option[A]] = getOption map (_.find(p))

  def exist(p: A => Boolean): P[Boolean] = getOption map (_.exists(p))

  def all(p: A => Boolean): P[Boolean] = getOption map (_.fold(true)(p))

  trait CompositionExperiment {
    import scalaz.~>

    def composeLens[Q[_], B <: A](ln: LensAlg[Q, B])(nat: Q ~> P) = new OptionalAlg[P, B] {
      implicit val M = self.M
      def getOption: P[Option[B]] =
        nat(ln.get) >>= (b => self.getOption.map(_.as(b)))
      def setOption(b: B): P[Option[Unit]] =
        nat(ln.put(b)) >> self.set(b) map (_.some)
    }
  }

  trait PrismAlgLaw {

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

  def prismAlgLaw = new PrismAlgLaw {}
}
