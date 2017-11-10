package org.hablapps.stateless
package core
package nat
package doobieImpl

import scalaz._, Scalaz._
import doobie.imports._

object LensFree{

  def fromSchemaKV[K,V](schema: DoobieSchemaKV[K,V]) =
    LensAlg[ReaderT[ConnectionIO,K,?],Free[LensF[V,?],?],V]{
      new (Free[LensF[V,?],?] ~> ReaderT[ConnectionIO,K,?]){
        def apply[T](q: Free[LensF[V,?],T]): ReaderT[ConnectionIO,K,T] =
          ReaderT[ConnectionIO,K,T]{
            k: K => q.foldMap(LensF.ToConnectionIO[K,V](schema)(k))
          }
      }
    }(Kleisli.kleisliMonadReader[ConnectionIO,K],implicitly)

}
