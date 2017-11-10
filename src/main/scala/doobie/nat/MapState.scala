package org.hablapps.stateless
package core
package nat
package doobieImpl

import scalaz._, Scalaz._
import shapeless._
import doobie.imports._
import doobie.postgres.imports._

object MapState{

  def fromSchemaV[K,V](schema: DoobieSchemaV[K,V]) =
    lib.MapAlg[ConnectionIO,State[V,?],K,V](
      op.At[ConnectionIO,State[Option[V],?],K,V]{
        (k: K) => LensAlg[ConnectionIO,State[Option[V],?],Option[V]]{
          λ[State[Option[V],?] ~> ConnectionIO]{
            q => schema.get(k).option >>= {
              q(_) match {
                case (None,out) => schema.remove(k).run.as(out)
                case (Some(v),out) => schema.insertOrUpdate(v).run.as(out)
              }
            }
          }
        }
      },
      op.FilterIndex[ConnectionIO,State[V,?],K,V]{
        (p: K => Boolean) => ITraversalAlg[ConnectionIO,State[V,?],K::HNil,V]{
          new (λ[t => (K::HNil => State[V,t])] ~> λ[t => ConnectionIO[List[t]]]){
            def apply[T](q: K::HNil => State[V,T]) =
              schema.getAll.list flatMap {
                _.traverse( g =>
                  if (p(schema.key(g)))
                    q(schema.key(g)::HNil)(g) match {
                      case (newG,out) =>
                        schema.insertOrUpdate(newG).run.as(List(out))
                    }
                  else List[T]().point[ConnectionIO]
                ).map(_.flatten)
            }
          }
        }
      }
    )


  def fromSchemaKV[K,V](schema: DoobieSchemaKV[K,V]) =
    lib.MapAlg[ConnectionIO,State[V,?],K,V](
      op.At[ConnectionIO,State[Option[V],?],K,V]{
        (k: K) => LensAlg[ConnectionIO,State[Option[V],?],Option[V]]{
          λ[State[Option[V],?] ~> ConnectionIO]{
            q => schema.get(k).option >>= {
              q(_) match {
                case (None,out) => schema.remove(k).run.as(out)
                case (Some(v),out) => schema.insertOrUpdate(k,v).run.as(out)
              }
            }
          }
        }
      },
      op.FilterIndex[ConnectionIO,State[V,?],K,V]{
        (p: K => Boolean) => ITraversalAlg[ConnectionIO,State[V,?],K::HNil,V]{
          new (λ[t => (K::HNil => State[V,t])] ~> λ[t => ConnectionIO[List[t]]]){
            def apply[T](q: K::HNil => State[V,T]) =
              schema.getIndices.list flatMap {
                _.traverse[ConnectionIO,List[T]]( k =>
                  if (p(k))
                    schema.get(k).unique >>= { value =>
                      q(k::HNil)(value) match {
                        case (newG,out) if newG != value =>
                          schema.insertOrUpdate(k,newG).run.as(List(out))
                        case (_, out) => List(out).point[ConnectionIO]
                      }
                    }
                  else List[T]().point[ConnectionIO]
                ).map(_.flatten)
              }
          }
        }
      }
    )

}