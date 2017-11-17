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
      // ).updateWithLogHandler(LogHandler.jdkLogHandler)
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
      // ).updateWithLogHandler(LogHandler.jdkLogHandler)
    )
}

trait DoobieSchemaV[K,V]{
  def key(v: V): K
  def insertOrUpdate(v: V): Update0
  def get(k: K): Query0[V]
  def getAll: Query0[V]
  def remove(k: K): Update0
}

trait DoobieSchemaKV[K,V]{
  def createTable: Update0
  def dropTable: Update0
  def insert(v: V): Update0
  def insertOrUpdate(k: K, v: V): Update0
  def getIndices: Query0[K]
  def get(k: K): Query0[V]
  def getAll: Query0[(K,V)]
  def remove(k: K): Update0
}
