package tyqu

import scala.annotation.targetName


case class QueryBuilder[T <: Scope](scope: T, from: String, where: Array[Expression[Boolean]]):

  @targetName("mapToScope")
  def map[T2 <: Scope](fn: T => T2): QueryBuilder[T2] =
    new QueryBuilder(fn(scope), from, where)
  
  @targetName("mapToTuple")
  inline transparent def map[T2 <: Tuple](inline fn: T => T2) =
    ScopeFactory.create(
      fn(scope),
      from,
      where,
    )

  @targetName("mapToTuple1")
  inline transparent def map[V](inline fn: T => Expression[V]) =
    ScopeFactory.create(
      Tuple1(fn(scope)),
      from,
      where,
    )

  def filter(predicate: T => Expression[Boolean]): QueryBuilder[T] =
    new QueryBuilder(scope, from, where :+ predicate(scope))

end QueryBuilder


type ColumnsToExpressions[T <: Tuple] <: Tuple =
  T match
    case EmptyTuple => EmptyTuple
    case Column[r, n] *: t => ColumnValue[r, n] *: ColumnsToExpressions[t]

def columnsToExpressions[T <: Tuple](columns: T, relation: String): ColumnsToExpressions[T] =
  columns match
    case _: EmptyTuple => EmptyTuple
    case t: (Column[r, n] *: tailType) =>
      ColumnValue[r, n](t.head.alias, t.head.name, relation) *: columnsToExpressions(t.tail, relation)

transparent inline def from[T <: Tuple](table: Table[T]) =
  ScopeFactory.create(
    columnsToExpressions(table.columns, table.tableName),
    table.tableName,
    Array.empty,
  )
