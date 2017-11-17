package org.hablapps.stateless
package core
package nat
package doobieImpl

import scalaz._, Scalaz._
import doobie.imports._
import monocle.{Lens => MLens}

object TraversalDoobie{

  // def fromDTraversal[K,V](dtrav: DoobieTraversal[K,V]) =
  //   TraversalAlg[StateT[ConnectionIO,K,?],State[V,?],V]{
  //     new (State[V,?] ~> λ[T => StateT[ConnectionIO,K,Option[T]]]){
  //       def apply[T](q: State[V,T]): StateT[ConnectionIO,K,Option[T]] =
  //         StateT[ConnectionIO,K,Option[T]]{
  //           k: K => dtrav.getOption(k).unique >>= {
  //             _.fold((k, Option.empty[T]).point[ConnectionIO])(
  //               q(_) match {
  //                 case (v,out) => dtrav.set(k)(v).run.as((k,Some(out)))
  //               })
  //             }
  //         }
  //       }
  //   }(StateT.stateTMonadState[K,ConnectionIO], implicitly)

  def fromStateT[K1,K2](dtrav: DoobieTraversal[K2,K1]) =
    TraversalAlg[StateT[ConnectionIO,K1,?],StateT[ConnectionIO,K2,?],K2]{
      new (StateT[ConnectionIO,K2,?] ~> λ[T=>StateT[ConnectionIO,K1,List[T]]]){
        def apply[T](q: StateT[ConnectionIO,K2,T]): StateT[ConnectionIO,K1,List[T]] =
          StateT[ConnectionIO,K1,List[T]]{
            k: K1 => dtrav.getList(k) >>= { 
              _.traverseU{
                q(_).map{ case (_,out) => List(out) }
              }.map(results => (k,results.flatten))
            }
          }
      }
    }(StateT.stateTMonadState[K1,ConnectionIO],StateT.stateTMonadState[K2,ConnectionIO])

}
