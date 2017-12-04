package org.hablapps.stateless
package core
package nat
package doobieImpl

import scalaz._, Scalaz._
import doobie.imports._
import monocle.{Lens => MLens}

object TraversalDoobie{

  def fromStateT[K1,K2](dtrav: DoobieTraversal[K2,K1]) =
    TraversalAlg[StateT[ConnectionIO,K1,?],StateT[ConnectionIO,K2,?],K2]{
      new (StateT[ConnectionIO,K2,?] ~> λ[T=>StateT[ConnectionIO,K1,List[T]]]){
        def apply[T](q: StateT[ConnectionIO,K2,T]) =
          StateT[ConnectionIO,K1,List[T]]{
            k: K1 => dtrav.getList(k) >>= { 
              _.traverseU{q(_).map(_._2)}.map((k,_))
            }
          }
      }
    }(StateT.stateTMonadState[K1,ConnectionIO],StateT.stateTMonadState[K2,ConnectionIO])

}
