package org.hablapps.stateless
package core
package nat
package doobieImpl

import scalaz._, Scalaz._
import shapeless._
import doobie.imports._
import doobie.postgres.imports._

object MapFree{

  def fromSchemaKV[K,V](schema: DoobieSchemaKV[K,V]) =
    lib.MapAlg[ConnectionIO,Free[LensF[V,?],?],K,V](
      op.At[ConnectionIO,Free[LensF[Option[V],?],?],K,V]{
        (k: K) => LensAlg[ConnectionIO,Free[LensF[Option[V],?],?],Option[V]]{
          λ[Free[LensF[Option[V],?],?] ~> ConnectionIO]{
            _.foldMap(LensF.OptionToConnectionIO[K,V](schema)(k))
          }
        }
      },
      op.FilterIndex[ConnectionIO,Free[LensF[V,?],?],K,V]{
        (p: K => Boolean) =>
          ITraversalAlg[ConnectionIO,Free[LensF[V,?],?],K::HNil,V]{
            new (λ[t => (K::HNil => Free[LensF[V,?],t])] ~> λ[t => ConnectionIO[List[t]]]){
              def apply[T](q: K::HNil => Free[LensF[V,?],T]) =
                schema.getIndices.list flatMap {
                  _.traverse[ConnectionIO,List[T]]( k =>
                    if (p(k))
                      q(k::HNil).foldMap(LensF.ToConnectionIO[K,V](schema)(k)).map(List(_))
                    else List[T]().point[ConnectionIO]
                  ).map(_.flatten)
                }
            }
          }
      }
    )
}