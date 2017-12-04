package org.hablapps.stateless
package test

import scalaz.State

import core.nat._

// View layer

trait View[P[_]] {
  def modifyZip(f: Int => Int): P[Unit]
}

object View {

  def fromData[P[_]](System: SystemData[P]) =
    new View[P]{
      def modifyZip(f: Int => Int): P[Unit] =
        (System.department composeTraversal
         System.Department.members composeOptional
         System.Department.Person.optAddress composeLens
         System.Department.Person.Address.zip).modify(f)
    }
}
