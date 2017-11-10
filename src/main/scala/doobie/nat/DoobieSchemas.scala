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
      (key: K) => sql"""
        SELECT $valueField FROM $table where $keyField=$key
      """.query[V],
      (key: K) => (value: V) => sql"""
        INSERT INTO $table VALUES ($key,$value)
        ON CONFLICT (keyField) DO UPDATE SET valueField = excluded.valueField;
      """.update
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
