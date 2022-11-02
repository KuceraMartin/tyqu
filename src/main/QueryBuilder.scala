package tyqu

import scala.annotation.targetName


case class QueryBuilder[T <: Scope](scope: T, from: String, where: Array[Expression[Boolean]], orderBy: List[OrderBy]):

  @targetName("mapToScope")
  def map[T2 <: Scope](fn: T => T2): QueryBuilder[T2] =
    new QueryBuilder(fn(scope), from, where, orderBy)
  
  @targetName("mapToTuple")
  inline transparent def map[T2 <: Tuple](inline fn: T => T2): QueryBuilder[?] =
    ScopeFactory.create(
      fn(scope),
      from,
      where,
      orderBy,
    )

  @targetName("mapToTuple1")
  inline transparent def map[V](inline fn: T => Expression[V]): QueryBuilder[?] =
    map(s => Tuple1(fn(s)))

  def filter(predicate: T => Expression[Boolean]): QueryBuilder[T] =
    new QueryBuilder(scope, from, where :+ predicate(scope), orderBy)

  @targetName("sortByWithTuple")
  def sortBy(fn: T => Tuple): QueryBuilder[T] =
    new QueryBuilder(scope, from, where, fn(scope).toList.asInstanceOf[List[OrderBy]])

  @targetName("sortByWithTuple1")
  def sortBy(fn: T => OrderBy): QueryBuilder[T] =
    sortBy(s => Tuple1(fn(s)))

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
    List.empty,
  )
