package org.hablapps.stateless
package smonocle
package nat

object all extends FoldState
    with GetterState
    with LensState
    with OptionalState
    with SetterState
    with TraversalState
    with ITraversalState
    with op.AtState
    with op.FilterIndexState
    with lib.MapState
