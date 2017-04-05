package org.hablapps.phoropter
package state

object all extends StateFold
  with StateGetter
  with StateLens
  with StateOptional
  with StatePrism
  with StateSetter
  with StateTraversal
  with indexed.StateMap
