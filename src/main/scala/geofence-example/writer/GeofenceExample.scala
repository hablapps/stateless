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

  type Q[X] = State[SGeofence, X]
  type PW[X] = Writer[List[ButtonPressed], X]
  type P[X] = StateT[PW, SGeofence, X]

  val iso: core.Iso.Aux[Geofence, Geofence.ADT, Monad] =
    Geofence.geofenceIso(
      LensAlg.lensIso[State[Region, ?], Region]
        .asInstanceOf[core.Iso.Aux[LensAlg[?[_], Region], LensAlg.ADT, Monad]],
      LensAlg.lensIso[State[Set[DID], ?], Set[DID]]
        .asInstanceOf[core.Iso.Aux[LensAlg[?[_], Set[DID]], LensAlg.ADT, Monad]])

  val circeIso: core.CirceSerializer[Geofence.ADT] =
    Geofence.geofenceCirceSerializer(
      LensAlg.lensCirceSerializer[State[Region, ?], Region]
        .asInstanceOf[core.CirceSerializer[LensAlg.ADT]],
      LensAlg.lensCirceSerializer[State[Set[DID], ?], Set[DID]]
        .asInstanceOf[core.CirceSerializer[LensAlg.ADT]])

  implicit val MS = StateT.stateTMonadState[SGeofence, PW]

  val geo: Geofence[P] = GenWriter.forAPIStateTWriterT[Geofence, Id, SGeofence](iso)(circeIso, coreGeo)

}

object GeofenceExampleRun extends App {
  import GeofenceExample._

  val res = progGen2(geo)(MS).eval(SGeofence(1, Set(2, 3, 4)))

  println(s"RES: $res")
  println(s"RES-VALUE: ${res.value}")
  println(s"RES-WRITTEN: ${res.written}")
}
