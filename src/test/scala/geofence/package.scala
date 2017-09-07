package org.hablapps.stateless
package test

import Math.abs

package object `geofence` {

  type GID = Int // geofence id
  type DID = Int // device id
  type Position = (Int, Int)
  type Time = Int
  type Radius = Int
  type Region = (Radius, Position)

  sealed abstract class OutputEvent
  case class Enter(did: DID) extends OutputEvent
  case class Exit(did: DID)  extends OutputEvent

  def inRegion(pos: Position, reg: Region): Boolean = (pos, reg) match {
    case ((x2, y2), (r, (x, y))) => (abs(x - x2) <= r) && (abs(y - y2) <= r)
  }
}
