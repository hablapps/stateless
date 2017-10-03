package org.hablapps.stateless
package kafka
package nat

import org.apache.kafka.clients.producer.{Producer, ProducerRecord}
import scala.concurrent.{blocking, Await, ExecutionContext, Future}
import scala.concurrent.duration._
import scalaz._
import scalaz.std.list._
import scalaz.std.scalaFuture._
import scalaz.syntax.id._

object GenKafka {

  type JSON = writer.nat.GenWriter.ButtonPressed

  def stateT[TC[_[_]], S](
      iso: core.Iso[TC],
      internal: TC[StateT[Writer[List[JSON], ?], S, ?]],
      producer: Producer[Unit, String])(implicit EC: ExecutionContext): TC[StateT[Future, (Producer[Unit, String], S), ?]] = {

    import iso._

    type P[X] = StateT[Writer[List[JSON], ?], S, X] // TODO(jfuentes): Add the producer in the state to remove it from signature
    type Q[X] = StateT[Future, (Producer[Unit, String], S), X]

    val qToP = new (Q ~> P) {
      def apply[B](qb: Q[B]): P[B] = StateT[Writer[List[JSON], ?], S, B] { s =>
        val ((p, s2), o) = Await.result(qb.run((producer, s)), 3 seconds)
        Writer(List.empty, (s2, o))
      }
    }
    val pToQ = new (P ~> Q) {
      def apply[B](pb: P[B]): Q[B] = StateT[Future, (Producer[Unit, String], S), B] { case (p, s) =>
        val (w, (s2, o)) = pb.run(s).run
        w.foldLeft(Future.successful(((p, s2), o))) { case (acc, evt) =>
          for {
            res <- acc
            _ <- Future { blocking {
                p.send(new ProducerRecord("test", evt.input)).get
              }}
          } yield res
        }
      }
    }
    val pToQNoop = new (P ~> Q) {
      def apply[B](pb: P[B]): Q[B] = StateT[Future, (Producer[Unit, String], S), B] { case (p, s) =>
        val (_, (s2, o)) = pb.run(s).run
        Future.successful(((p, s2), o))
      }
    }

    def natQ(natP: ADT[P, ?] ~> P) = new (ADT[Q, ?] ~> Q) {
      def apply[A](adtQ: ADT[Q, A]): Q[A] = {
        val adtP = mapHK(qToP)(adtQ)
        natP(adtP) |>
          (if (kind(adtP) == core.Iso.Command)
            pToQ
          else
            pToQNoop)
      }
    }

    to[P](internal) |> (natQ(_)) |> from[Q]
  }

}
