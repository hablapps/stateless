package org.hablapps.phoropter
package core
package indexed

import scalaz.syntax.functor._

import op.At
import At.syntax._

trait MonadMap[P[_], Q[_], I, A] extends MonadITraversal[P, Q, I, A] {

  implicit val ev: At[P, Q, I, A]

  // additional algebra

  def pick[O](i: I)(qo: Q[O]): P[Option[O]] = at(i).hom(qo.map(Option(_)))

  def get(i: I): P[Option[A]] = at(i).get
}
