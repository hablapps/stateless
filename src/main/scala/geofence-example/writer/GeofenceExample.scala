package org.hablapps.stateless
package writer
package nat

import scalaz._, Id.Id
import scalaz.std.list._
import scalaz.syntax.id._

import core.nat.LensAlg
import core.nat.GeofenceExample.{SGeofence, geo => coreGeo}
import geofence._, Programs._
import GenWriter.ButtonPressed

object GeofenceExample {

  type P[X] = State[SGeofence, X]
  type QW[X] = Writer[List[ButtonPressed], X]
  type Q[X] = StateT[QW, SGeofence, X]

  val iso: core.Iso.Aux[Geofence, Geofence.ADT] =
    Geofence.geofenceIso(
      LensAlg.lensIso[State[Region, ?], Region]
        .asInstanceOf[core.Iso.Aux[LensAlg[?[_], Region], LensAlg.ADT]],
      LensAlg.lensIso[State[Set[DID], ?], Set[DID]]
        .asInstanceOf[core.Iso.Aux[LensAlg[?[_], Set[DID]], LensAlg.ADT]])

  val nat = λ[Q ~> P] { px =>
    StateT { sg => px.run(sg).value }
  }

  val geo: Geofence[Q] = GenWriter.forAPIStateTWriterT[Geofence, Id, SGeofence](iso, coreGeo)(nat)

}

object GeofenceExampleRun extends App {
  import GeofenceExample._

  val res = progGen2(geo)(StateT.stateTMonadState[SGeofence, QW]).eval(SGeofence(1, Set(2, 3, 4)))

  println(s"RES: $res")
  println(s"RES-VALUE: ${res.value}")
  println(s"RES-WRITTEN: ${res.written}")
}
