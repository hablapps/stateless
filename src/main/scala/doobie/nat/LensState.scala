package org.hablapps.stateless
package core
package nat
package doobieImpl

import scalaz._, Scalaz._
import doobie.imports._
import monocle.{Lens => MLens}

object LensState{

  def fromDLens[K,V](dlens: DoobieLens[K,V]) =
    LensAlg[StateT[ConnectionIO,K,?],State[V,?],V]{
      new (State[V,?] ~> StateT[ConnectionIO,K,?]){
        def apply[T](q: State[V,T]): StateT[ConnectionIO,K,T] =
          StateT[ConnectionIO,K,T]{
            k: K => dlens.get(k).unique >>= {
              q(_) match {
                case (v,out) => dlens.set(k)(v).run.as((k,out))
              }
            }
          }
      }
    }(StateT.stateTMonadState[K,ConnectionIO],implicitly)

  def fromStateT[K1,K2](dlens: DoobieLens[K1,K2]) =
    LensAlg[StateT[ConnectionIO,K1,?],StateT[ConnectionIO,K2,?],K2]{
      new (StateT[ConnectionIO,K2,?] ~> StateT[ConnectionIO,K1,?]){
        def apply[T](q: StateT[ConnectionIO,K2,T]): StateT[ConnectionIO,K1,T] =
          StateT[ConnectionIO,K1,T]{
            k: K1 => dlens.get(k).unique >>= {
              q(_) >>= {
                case (v,out) => dlens.set(k)(v).run.as((k,out))
              }
            }
          }
      }
    }(StateT.stateTMonadState[K1,ConnectionIO],StateT.stateTMonadState[K2,ConnectionIO])

}
