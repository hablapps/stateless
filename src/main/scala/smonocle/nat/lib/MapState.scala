package org.hablapps.stateless
package smonocle
package nat
package lib

import scalaz._, Scalaz._

import monocle.{ Lens => MLens }

import core.nat.lib.MapAlg

trait MapState {

  implicit def mapFromMapMLens[F[_]: Monad, S, K, V](ln: MLens[S, Map[K, V]])
      : MapAlg.Aux[StateT[F, S, ?], StateT[F, V, ?], K, V] =
    MapAlg[StateT[F, S, ?], StateT[F, V, ?], K, V](
      all.atFromMapMLens[F, S, K, V](ln),
      all.filterIndexFromMapMLens[F, S, K, V](ln))
}
