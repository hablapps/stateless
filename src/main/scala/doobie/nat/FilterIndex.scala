package org.hablapps.stateless
package core
package nat
package doobieImpl

import scalaz._, Scalaz._
import doobie.imports._
import shapeless._

import op.FilterIndex

object FilterIndexDoobie {

  def fromState[K1, K2, V](dfi: DoobieFilterIndex[K1, K2, V]) =
    FilterIndex[StateT[ConnectionIO, K1, ?], State[V, ?], K2, V] { p =>
      ITraversalAlg[StateT[ConnectionIO, K1, ?], State[V, ?], K2 :: HNil, V](
        new (λ[x => K2 :: HNil => State[V, x]] ~> λ[y => StateT[ConnectionIO, K1, List[y]]]) {
          def apply[X](q: K2 :: HNil => State[V, X]) =
            StateT[ConnectionIO, K1, List[X]] { k1 =>
              dfi.getAll(k1).list >>= {
                _.traverse { case (k2, v) =>
                  if (p(k2))
                    q(k2 :: HNil)(v) match {
                      case (newG, out) =>
                        dfi.insertOrUpdate(k1)(k2)(newG).run.as(Option(out))
                    }
                  else Option.empty[X].point[ConnectionIO]
                }.map(xs => (k1, xs.flatten))
              }
            }
        }
      )(StateT.stateTMonadState[K1, ConnectionIO], implicitly)
    }
}
