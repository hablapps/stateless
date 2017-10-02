package org.hablapps.stateless
package writer
package nat

import scalaz._
import scalaz.Id.Id
import scalaz.syntax.id._
import scalaz.syntax.functor._
import scalaz.syntax.monadTell._
import scalaz.std.list._

object GenWriter {

  trait forAPIStateTWriterT[TC[_[_]], R[_], S] {

    /* EVIDENCES */

    val iso: core.Iso[TC]
    implicit val monadR: Monad[R]

    /* TYPES */

    type  P[X] = StateT[R, S, X]
    type  W    = List[String]
    type QF[X] = WriterT[R, W, X]
    type  Q[X] = StateT[QF, S, X]

    /* DERIVED */

    val monadQ: Monad[Q] = StateT.stateTMonadState[S, QF]

    def go(
        orig: TC[StateT[R, S, ?]],
        qToP: Q ~> P): TC[Q] = {

      val pToQ: P ~> Q = new (P ~> Q) {
        def apply[A](pa: P[A]): Q[A] = StateT[QF, S, A] { s =>
          val res0: R[(S, A)] = pa.run(s)
          WriterT.put(res0)(List.empty)
        }
      }
      val ev: MonadTell[Q, W] = new MonadTell[Q, W] {
        def point[A](a: => A): Q[A] = monadQ.point(a)
        def bind[A, B](fa: Q[A])(f: A => Q[B]): Q[B] = monadQ.bind(fa)(f)

        def writer[A](w: W, v: A): Q[A] = StateT[QF, S, A] { s =>
          (w, (s, v)) |> (monadR.point(_)) |> WriterT.writerT
        }
      }

      forAPIGen[TC, P, Q](orig, iso)(pToQ, qToP)(ev)
    }
  }

  object forAPIStateTWriterT {
    type Q[R[_], S, X] = StateT[WriterT[R, List[String], ?], S, X]
    def apply[TC[_[_]], R[_]: Monad, S](
        iso2: core.Iso[TC],
        orig: TC[StateT[R, S, ?]])(
        qToP: Q[R, S, ?] ~> StateT[R, S, ?]) =
      (new {
        val iso: iso2.type = iso2
        val monadR = Monad[R]
      } with forAPIStateTWriterT[TC, R, S]).go(orig, qToP)
  }

  def forAPIGen[TC[_[_]], P[_], Q[_]: MonadTell[?[_], List[String]]](
      internal: TC[P],
      iso: core.Iso[TC])(
      pToQ: P ~> Q,
      qToP: Q ~> P): TC[Q] = {

    import iso.{to, from, dimapHK}

    val transf = λ[λ[α=>(iso.ADT[Q, α], Q[α])] ~> Q] { case (adt, qx) =>
      qx flatMap { out =>
        out.point[Q] :++> List(s"$adt : Output : $out")
      }
    }

    to[P](internal) |> (iso.dimapHK[P, Q](qToP, pToQ, iso.recover(transf))(_)) |> from[Q]
  }

}
