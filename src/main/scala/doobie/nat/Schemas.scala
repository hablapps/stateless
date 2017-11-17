package org.hablapps.stateless
package core
package nat
package doobieImpl

import doobie.imports._

case class DoobieLens[K,V](
  get: K => Query0[V],
  set: K => V => Update0
)

object DoobieLens{
  def apply[K: Param, V: Param: Composite](
    table: String, keyField: String, valueField: String
  ): DoobieLens[K,V] =
    DoobieLens[K,V](
      (key: K) => (
        fr"SELECT" ++ Fragment.const(valueField) ++
        fr"FROM" ++ Fragment.const(table) ++
        fr"WHERE" ++ Fragment.const(keyField) ++
        fr"=$key"
      ).query[V],
      (key: K) => (value: V) => (
        fr"UPDATE" ++ Fragment.const(table) ++
        fr"SET" ++ Fragment.const(valueField) ++ fr"=" ++ fr"$value" ++
        fr"WHERE" ++ Fragment.const(keyField) ++ fr"=" ++ fr"$key"
      ).update
    )
}

case class DoobieOptional[K,V](
  getOption: K => Query0[Option[V]],
  set: K => V => Update0
)

object DoobieOptional{
  def apply[K: Param, V: Param](
    table: String, keyField: String, valueField: String
  )(implicit C: Composite[Option[V]]): DoobieOptional[K,V] =
    DoobieOptional[K,V](
      (key: K) => (
        fr"SELECT" ++ Fragment.const(valueField) ++
        fr"FROM" ++ Fragment.const(table) ++
        fr"WHERE" ++ Fragment.const(keyField) ++
        fr"=$key"
      ).query[Option[V]],
      (key: K) => (value: V) => (
        fr"UPDATE" ++ Fragment.const(table) ++
        fr"SET" ++ Fragment.const(valueField) ++ fr"=" ++ fr"$value" ++
        fr"WHERE" ++ Fragment.const(keyField) ++ fr"=" ++ fr"$key"
      ).update
    )
}

case class DoobieTraversal[K,V](
  getList: V => ConnectionIO[List[K]]
)

object DoobieTraversal{
  def apply[K: Param: Composite, V: Param](
    table: String, keyField: String, valueField: String
  ): DoobieTraversal[K,V] =
    DoobieTraversal[K,V](
      (value: V) => (
        fr"SELECT" ++ Fragment.const(keyField) ++
        fr"FROM" ++ Fragment.const(table) ++
        fr"WHERE" ++ Fragment.const(valueField) ++ fr"=$value"
      ).query[K].list
    )
}
