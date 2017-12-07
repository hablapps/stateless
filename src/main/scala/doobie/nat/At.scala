package org.hablapps.stateless
package core
package nat
package doobieImpl

import scalaz._, Scalaz._
import doobie.imports._

import op.At

object AtDoobie {

  def fromState[K1, K2, V](dat: DoobieAt[K1, K2, V]) =
    At[StateT[ConnectionIO, K1, ?], State[Option[V], ?], K2, V] { k2 =>
      LensAlg[StateT[ConnectionIO, K1, ?], State[Option[V], ?], Option[V]](
        new (State[Option[V], ?] ~> StateT[ConnectionIO, K1, ?]) {
          def apply[X](q: State[Option[V], X]) =
            StateT[ConnectionIO, K1, X] { k1 =>
              dat.get(k1)(k2).option >>= { ov =>
                q(ov) match {
                  case (None, out) => dat.remove(k1)(k2).run.as((k1, out))
                  case (Some(v), out) => {
                    dat.remove(k1)(k2).run *>
                    dat.insert(k1)(k2)(v).run.as((k1, out))
                  }
                }
              }
            }
        }
      )(StateT.stateTMonadState[K1, ConnectionIO], implicitly)
    }
}
