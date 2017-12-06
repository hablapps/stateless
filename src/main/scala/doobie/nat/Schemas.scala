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
    table: String, keyField: String, valField: String
  ): DoobieLens[K,V] =
    DoobieLens[K,V](
      (key: K) => (
        fr"SELECT" ++ Fragment.const(valField) ++
        fr"FROM" ++ Fragment.const(table) ++
        fr"WHERE" ++ Fragment.const(keyField) ++
        fr"=$key"
      ).query[V],
      (key: K) => (value: V) => (
        fr"UPDATE" ++ Fragment.const(table) ++
        fr"SET" ++ Fragment.const(valField) ++ fr"=" ++ fr"$value" ++
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
    table: String, keyField: String, valField: String
  )(implicit C: Composite[Option[V]]): DoobieOptional[K,V] =
    DoobieOptional[K,V](
      (key: K) => (
        fr"SELECT" ++ Fragment.const(valField) ++
        fr"FROM" ++ Fragment.const(table) ++
        fr"WHERE" ++ Fragment.const(keyField) ++
        fr"=$key"
      ).query[Option[V]],
      (key: K) => (value: V) => (
        fr"UPDATE" ++ Fragment.const(table) ++
        fr"SET" ++ Fragment.const(valField) ++ fr"=" ++ fr"$value" ++
        fr"WHERE" ++ Fragment.const(keyField) ++ fr"=" ++ fr"$key"
      ).update
    )
}

case class DoobieTraversal[K,V](
  getList: V => ConnectionIO[List[K]]
)

object DoobieTraversal{
  def apply[K: Param: Composite, V: Param](
    table: String, keyField: String, valField: String
  ): DoobieTraversal[K,V] =
    DoobieTraversal[K,V](
      (value: V) => (
        fr"SELECT" ++ Fragment.const(keyField) ++
        fr"FROM" ++ Fragment.const(table) ++
        fr"WHERE" ++ Fragment.const(valField) ++ fr"=$value"
      ).query[K].list
    )
}

case class DoobieAt[K1, K2, V](
  get: K1 => K2 => Query0[V],
  insert: K1 => K2 => V => Update0,
  remove: K1 => K2 => Update0)

object DoobieAt {
  def apply[K1: Param : Composite, K2: Param : Composite, V: Param : Composite](
      table: String,
      key1Field: String,
      key2Field: String,
      valField: String): DoobieAt[K1, K2, V] =
    DoobieAt[K1, K2, V](
      (k1: K1) => (k2: K2) => (
        fr"SELECT" ++ Fragment.const(valField) ++
        fr"FROM" ++ Fragment.const(table) ++
        fr"WHERE" ++ Fragment.const(key1Field) ++ fr"=$k1" ++
        fr"AND" ++ Fragment.const(key2Field) ++ fr"=$k2"
      ).query[V],
      (k1: K1) => (k2: K2) => (v: V) => (
        fr"INSERT INTO" ++ Fragment.const(table) ++
        fr"(" ++ Fragment.const(valField) ++ fr"," ++
                 Fragment.const(key1Field) ++ fr"," ++
                 Fragment.const(key2Field) ++ fr")" ++
        fr"VALUES" ++
        fr"(" ++ fr"$v"++ fr"," ++ fr"$k1"++ fr"," ++ fr"$k2" ++ fr")"
      ).update,
      (k1: K1) => (k2: K2) => (
        fr"DELETE FROM" ++ Fragment.const(table) ++
        fr"WHERE" ++ Fragment.const(key1Field) ++ fr"=" ++ fr"$k1" ++
        fr"AND" ++ Fragment.const(key2Field) ++ fr"=$k2"
      ).update)
}

case class DoobieFilterIndex[K1, K2, V](
  getAll: K1 => Query0[(K2, V)],
  insertOrUpdate: K1 => K2 => V => Update0)

object DoobieFilterIndex {
  def apply[K1: Param : Composite, K2: Param : Composite, V: Param : Composite](
      table: String,
      key1Field: String,
      key2Field: String,
      valField: String): DoobieFilterIndex[K1, K2, V] =
    DoobieFilterIndex[K1, K2, V](
      (k1: K1) => (
        fr"SELECT (" ++ Fragment.const(valField) ++ fr"," ++
                        Fragment.const(key2Field) ++ fr")" ++
        fr"FROM" ++ Fragment.const(table) ++
        fr"WHERE" ++ Fragment.const(key1Field) ++ fr"=$k1"
      ).query[(K2, V)],
      (k1: K1) => (k2: K2) => (v: V) => (
        fr"UPDATE" ++ Fragment.const(table) ++
        fr"SET" ++ Fragment.const(valField) ++ fr"=" ++ fr"$v" ++
        fr"WHERE" ++ Fragment.const(key1Field) ++ fr"=" ++ fr"$k1" ++
        fr"AND" ++ Fragment.const(key2Field) ++ fr"=$k2"
      ).update)
}
