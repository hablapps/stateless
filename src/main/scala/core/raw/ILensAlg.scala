package org.hablapps.stateless
package core
package raw

import scalaz.Monad
import scalaz.syntax.std.option._

trait ILensAlg[P[_], I, A] extends Monad[P] {

  def get: P[(I, A)]

  def set(a: A): P[Unit]

  /* derived methods */

  def modify(f: A => A): P[Unit] = bind(get) { case (_, a) => set(a) }

  def find(p: ((I, A)) => Boolean): P[Option[(I, A)]] =
    map(get)(a => if (p(a)) a.some else None)

  def exist(p: ((I, A)) => Boolean): P[Boolean] = map(get)(p)
}
