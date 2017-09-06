package org.hablapps.stateless
package test
package geofence

import Function.const

import scalaz._, Scalaz._

import shapeless._

trait GeofenceView[P[_]] {

  def add(reg: Region): P[GID]

  def remove(gid: GID): P[Unit]

  def at(did: DID, pos: Position): P[List[(GID, OutputEvent)]]

  def tick(time: Time): P[List[((GID, DID), Time)]]

  def destroy: P[Unit]
}

object GeofenceView {

  def fromData[S, P[_]](
      sys: System.Aux[S, P])(implicit
      ev0: Monad[P],
      ev1: Monad[sys.geofence.P],
      ev2: Monad[sys.timer.P]): GeofenceView[P] = new GeofenceView[sys.P] {
    import sys.{ P => _, _ }
    import sys.timer.{ P => _, _ }
    import sys.geofence.{ P => _, _ }

    def add(reg: Region): P[GID] =
      for {
        gid <- counterLn.get
        _   <- counterLn.modify(_ + 1)
        _   <- geofenceMp(gid).set(Option(fromRegion(reg)))
      } yield gid

    def remove(gid: GID): P[Unit] =
      for {
        _  <- geofenceMp(gid).set(None)
        tr = timerLn.asIndexed.composeTraversal(alarmMp.filterIndex(_._1 == gid))
        ks <- tr.indexes
        _  <- ks.traverse(k => timerLn.composeLens(alarmMp(k.head)).set(None))
      } yield ()

    def at(did: DID, pos: Position): P[List[(GID, OutputEvent)]] =
      for {
        mevs <- geofenceMp.hom(getEvent(did, pos).strengthL(_))
        evs  = mevs.foldRight(List.empty[(GID, OutputEvent)]) {
          case ((gid, oev), acc) => oev.fold(acc)((gid.head, _) :: acc)
        }
        _ <- timerLn.hom(evs.traverse(tp => setAlarm(tp._1, tp._2)))
      } yield evs

    private def getEvent(
        did: DID,
        pos: Position): geofence.P[Option[OutputEvent]] =
      for {
        reg  <- regionLn.get
        cnd1 <- insideLn.gets(_.contains(did))
        cnd2 =  inRegion(pos, reg)
        res  <- (cnd1, cnd2) match {
          case (true, false) =>
            insideLn.modify(_ - did) >> Option(Exit(did)).point[geofence.P]
          case (false, true) =>
            insideLn.modify(_ + did) >> Option(Enter(did)).point[geofence.P]
          case _ => None.point[geofence.P]
        }
      } yield res

    private def setAlarm(gid: GID, ev: OutputEvent): timer.P[Unit] = ev match {
      case Enter(did) => currentLn.get >>= (t => alarmMp((gid, did)).set(Option(t + 3)))
      case Exit(did)  => alarmMp((gid, did)).set(None)
    }

    def tick(time: Time): P[List[((GID, DID), Time)]] =
      for {
        _   <- timerLn.composeLens(currentLn).set(time)
        off <- timerLn.asIndexed.composeTraversal(alarmMp).foldMap {
          case (i, t) => if (t <= time) List.empty else List((i.head, t))
        }
      } yield off

    def destroy: P[Unit] =
      for {
        _ <- geofenceMp.destroy
        _ <- timerLn.hom(alarmMp.destroy)
      } yield ()
  }
}
