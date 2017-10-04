package org.hablapps.stateless
package core
package nat

import scalaz.State

import geofence._, Programs._

object GeofenceExample {

  type P[A] = State[SGeofence, A]

  case class SGeofence(region: Region, inside: Set[DID])

  val geo = new Geofence[P] {
    val regionLn: LensAlg[P, Region] = LensAlg.state[SGeofence, Region](_.region, r => _.copy(region = r))
    val insideLn: LensAlg[P, Set[DID]] = LensAlg.state[SGeofence, Set[DID]](_.inside, i => _.copy(inside = i))
  }

}

object GeofenceExampleRun extends App {
  import GeofenceExample._

  val res1 = progGen(geo).run(SGeofence(1, Set(2, 3, 4)))
  val res2 = progGen2(geo).run(SGeofence(1, Set(2, 3, 4)))

  println(s"""|RES1: $res1
              |RES2: $res2""".stripMargin)

}
