package org.hablapps.stateless
package core
package raw

import scalaz.{ Equal, Monad, MonadReader }
import scalaz.syntax.equal._
import scalaz.syntax.monad._
import scalaz.syntax.std.option._

trait GetterAlg[P[_], A] extends MonadReader[P, A] { self =>

  /* derived methods */

  def get: P[A] = ask

  // XXX: we should be extending `MonadAsk` instead
  def local[X](f: A => A)(px: P[X]) = ???

  def find(p: A => Boolean): P[Option[A]] =
    map(get)(a => if (p(a)) a.some else None)

  def exist(p: A => Boolean): P[Boolean] = asks(p)

  trait GetterAlgLaw {
    implicit val _: Monad[P] = self

    def getGet(implicit eq: Equal[P[(A, A)]]): Boolean =
      (get >>= (a1 => get >>= (a2 => (a1, a2).point[P]))) ===
        (get >>= (a => (a, a).point[P]))
  }

  def getterAlgLaw = new GetterAlgLaw {}
}
