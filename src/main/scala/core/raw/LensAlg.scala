package org.hablapps.stateless
package core
package raw

import scalaz.{ Equal, Monad, MonadState }
import scalaz.syntax.monad._
import scalaz.syntax.equal._
import scalaz.syntax.std.option._

trait LensAlg[P[_], A] extends MonadState[P, A] { self =>

  /* derived methods */

  def set(a: A): P[Unit] = put(a)

  def init: P[A] = get

  def find(p: A => Boolean): P[Option[A]] =
    map(get)(a => if (p(a)) a.some else None)

  def exist(p: A => Boolean): P[Boolean] = gets(p)

  trait LensAlgLaw {
    implicit val _: Monad[P] = self

    def getGet(implicit eq: Equal[P[(A, A)]]): Boolean =
      (get >>= (a1 => get >>= (a2 => (a1, a2).point[P]))) ===
        (get >>= (a => (a, a).point[P]))

    def getPut(implicit eq: Equal[P[Unit]]): Boolean =
      (get >>= put) === ().point[P]

    def putGet(a: A)(implicit eq: Equal[P[A]]): Boolean =
      (put(a) >> get) === (put(a) >> a.point[P])

    def putPut(a1: A, a2: A)(implicit eq: Equal[P[Unit]]): Boolean =
      (put(a1) >> put(a2)) === put(a2)
  }

  def lensAlgLaw = new LensAlgLaw {}
}
