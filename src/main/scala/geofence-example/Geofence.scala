package org.hablapps.stateless
package geofence

import scalaz.{Monad, ~>}
import scalaz.syntax.id._

import core.nat.LensAlg

trait Geofence[P[_]] {

  /* OPTICS */
  val regionLn: LensAlg[P, Region]
  val insideLn: LensAlg[P, Set[DID]]

  /* DERIVED */
  def addInside(did: DID): P[Unit] = insideLn.modify(_ + did)
  def removeInside(did: DID): P[Unit] = insideLn.modify(_ - did)

}

/* This whole companion object should be auto-generated by azúcar */
object Geofence {
  import core.{Iso, CirceIso}

  // type Aux = "regionLn" ->> regionLn.type :+: "insideLn" ->> insideLn.type :+: CNil
  // or...
  sealed abstract class ADT[P[_], A]
  case class RegionLn[P[_], A](internal: LensAlg.ADT[P, A]) extends ADT[P, A]
  case class InsideLn[P[_], A](internal: LensAlg.ADT[P, A]) extends ADT[P, A]
  case class AddInside[P[_]](did: DID) extends ADT[P, Unit]
  case class RemoveInside[P[_]](did: DID) extends ADT[P, Unit]

  def geofenceCirceIso(implicit
      regionIso: CirceIso[LensAlg.ADT],
      insideIso: CirceIso[LensAlg.ADT]): CirceIso[Geofence.ADT] =
    new  CirceIso[Geofence.ADT] {

      import io.circe.Json
      def toJSON[P[_], A](adt: ADT[P, A]): Json = adt match {
        case RegionLn(internal) =>
          Json.obj(
            "name" -> Json.fromString("RegionLn"),
            "internal" -> regionIso.toJSON(internal))
        case InsideLn(internal) =>
          Json.obj(
            "name" -> Json.fromString("InsideLn"),
            "internal" -> insideIso.toJSON(internal))
        case AddInside(did) =>
          Json.obj(
            "name" -> Json.fromString("AddInside"),
            "did" -> Json.fromLong(did))
        case RemoveInside(did) =>
          Json.obj(
            "name" -> Json.fromString("RemoveInside"),
            "did" -> Json.fromLong(did))
      }
      def fromJSON[P[_]](json: Json): ADT[P, _] =
        json.hcursor.downField("name").as[String] match {
          case Right("RegionLn") =>
            (for {
              internal <- json.hcursor.downField("internal").as[Json]
            } yield RegionLn(regionIso.fromJSON[P](internal))).getOrElse(???)
          case Right("InsideLn") =>
            (for {
              internal <- json.hcursor.downField("internal").as[Json]
            } yield InsideLn(insideIso.fromJSON[P](internal))).getOrElse(???)
          case Right("AddInside") =>
            json.hcursor.downField("did").as[Long]
              .map(AddInside[P](_))
              .getOrElse(???)
          case Right("RemoveInside") =>
            json.hcursor.downField("did").as[Long]
              .map(RemoveInside[P](_))
              .getOrElse(???)
          case _ => ??? // Deserialization error
        }
    }

  implicit def geofenceIso(implicit
      regionIso: Iso.Aux[LensAlg[?[_], Region], LensAlg.ADT],
      insideIso: Iso.Aux[LensAlg[?[_], Set[DID]], LensAlg.ADT]) =
    new GeofenceIsoClass

  class GeofenceIsoClass(implicit
      regionIso: Iso.Aux[LensAlg[?[_], Region], LensAlg.ADT],
      insideIso: Iso.Aux[LensAlg[?[_], Set[DID]], LensAlg.ADT])
      extends Iso[Geofence] {
    type ADT[P2[_], X] = Geofence.ADT[P2, X]

    def mapHK[P[_], Q[_]](nat: P ~> Q) = new (ADT[P, ?] ~> ADT[Q, ?]) {
      def apply[A](pa: ADT[P, A]): ADT[Q, A] = pa match {
        case RegionLn(internal) => RegionLn(internal |> regionIso.mapHK(nat).apply)
        case InsideLn(internal) => InsideLn(internal |> insideIso.mapHK(nat).apply)
        case AddInside(did) => AddInside(did)
        case RemoveInside(did) => RemoveInside(did)
      }
    }

    def kind[P[_], X](adt: ADT[P, X]): Iso.Kind = adt match {
      case RegionLn(internal) => regionIso.kind(internal)
      case InsideLn(internal) => insideIso.kind(internal)
      case AddInside(did) => Iso.Command
      case RemoveInside(did) => Iso.Command
    }

    def recover[P[_]: Monad](transf: λ[α=>(ADT[P, α], P[α])] ~> P) = λ[λ[α=>(ADT[P, α], P[α])] ~> P] { t => t._1 match {
      case RegionLn(internal) =>
        regionIso.recover[P](
          λ[λ[α=>(regionIso.ADT[P, α], P[α])] ~> P] { x => transf(RegionLn(x._1), x._2) }
          ).apply((internal, t._2))
      case InsideLn(internal) =>
        insideIso.recover[P](
          λ[λ[α=>(insideIso.ADT[P, α], P[α])] ~> P] { x => transf(InsideLn(x._1), x._2) }
          ).apply((internal, t._2))
      case _ =>
        transf(t)
    }}

    def to[P[_]](fp: Geofence[P]): ADT[P, ?] ~> P =
      new (ADT[P, ?] ~> P) {
        def apply[A](adtA: ADT[P, A]): P[A] = adtA match {
          case RegionLn(internal) => regionIso.to[P](fp.regionLn) |> { _(internal) }
          case InsideLn(internal) => insideIso.to[P](fp.insideLn) |> { _(internal) }
          case AddInside(did) => fp.addInside(did)
          case RemoveInside(did) => fp.removeInside(did)
        }
      }
    def from[P[_]](gp: ADT[P, ?] ~> P): Geofence[P] =
      new Geofence[P] {
        val regionLn: LensAlg[P, Region] =
          regionIso.from[P](new (LensAlg.ADT[P, ?] ~> P) {
            def apply[X](l: LensAlg.ADT[P, X]): P[X] =
              gp(RegionLn[P, X](l))
          })
          // regionIso.from[P](λ[LensAlg.ADT[P, Region, ?] ~> P] { l => RegionLn(l) |> gp })
        val insideLn: LensAlg[P, Set[DID]] =
          insideIso.from[P](new (LensAlg.ADT[P, ?] ~> P) {
            def apply[X](l: LensAlg.ADT[P, X]): P[X] =
              gp(InsideLn(l))
          })

        override def addInside(did: DID): P[Unit] = gp(AddInside(did))
        override def removeInside(did: DID): P[Unit] = gp(RemoveInside(did))
      }
  }

}
