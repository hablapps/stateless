package org.hablapps.stateless
package geofence

import scalaz.Monad
import scalaz.syntax.monad._

object Programs {

  def progGen[P[_]: Monad](geo: Geofence[P]) =
    for {
      _ <- geo.regionLn.get
      _ <- geo.insideLn.get
      _ <- geo.regionLn.put(5)
      _ <- geo.insideLn.put(Set(6, 7, 8))
      reg <- geo.regionLn.get
      ins <- geo.insideLn.get
    } yield (reg, ins)

  def progGen2[P[_]: Monad](geo: Geofence[P]) =
    for {
      _ <- geo.regionLn.modify(_ + 1)
      // _ <- geo.insideLn.modify(_ - 3)
      res1 <- geo.insideLn.get
      _ <- geo.removeInside(3)
      _ <- geo.addInside(10)
      res2 <- geo.insideLn.get
    } yield (res1, res2)

}
