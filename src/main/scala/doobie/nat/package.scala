package org.hablapps.stateless
package core
package nat

import scalaz._, Scalaz._
import doobie.imports._

package object doobieImpl{

  sealed abstract class LensF[A,T]
  case class Get[A]() extends LensF[A,A]
  case class Put[A](a: A) extends LensF[A,Unit]

  object LensF{

    implicit def MS[A] = new MonadState[Free[LensF[A,?],?],A]{
      def point[T](a: => T) =
        Free.point[LensF[A,?],T](a)

      def bind[B,C](p: Free[LensF[A,?],B])(f: B => Free[LensF[A,?],C]) =
        p flatMap f

      def get() =
        Free.liftF[LensF[A,?],A](Get())

      def put(a: A) =
        Free.liftF[LensF[A,?],Unit](Put(a))

      def init =
        get
    }

    def ToState[A] = λ[LensF[A,?] ~> State[A,?]]{
      case _ : Get[A] => State.get
      case Put(a1) => State.put(a1)
    }

    def ToConnectionIO[K,V](schema: DoobieSchemaKV[K,V])(k: K) = λ[LensF[V,?] ~> ConnectionIO]{
      case _ : Get[V] =>
        schema.get(k).unique
      case Put(a) =>
        schema.insertOrUpdate(k,a).run.void
    }

    def OptionToConnectionIO[K,V](schema: DoobieSchemaKV[K,V])(k: K) = λ[LensF[Option[V],?] ~> ConnectionIO]{
      case _ : Get[Option[V]] =>
        schema.get(k).option
      case put: Put[Option[V]] =>
        put.a.fold(schema.remove(k).run.void){
          a => schema.insertOrUpdate(k,a).run.void
        }
    }
  }
}