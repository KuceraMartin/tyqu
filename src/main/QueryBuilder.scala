package tyqu

import scala.annotation.targetName

import utils.checkTupleOf


case class QueryBuilder[T <: Scope](scope: T, from: String, where: Option[Expression[Boolean]], orderBy: List[OrderBy]):

  @targetName("mapToScope")
  def map[T2 <: Scope](fn: T => T2): QueryBuilder[T2] =
    new QueryBuilder(fn(scope), from, where, orderBy)
  
  @targetName("mapToTuple")
  inline transparent def map[T2 <: Tuple](inline fn: T => T2): QueryBuilder[?] =
    checkTupleOf[NamedExpression[_, _]](fn(scope))
    QueryBuilderFactory.create(
      fn(scope),
      from,
      where,
      orderBy,
    )

  @targetName("mapToTuple1")
  inline transparent def map[V, T2 <: String & Singleton](inline fn: T => NamedExpression[V, T2]): QueryBuilder[?] =
    map(s => Tuple1(fn(s)))

  def filter(predicate: T => Expression[Boolean]): QueryBuilder[T] =
    val expr = predicate(scope)
    val newWhere = where.map(_ && expr).getOrElse(expr)
    new QueryBuilder(scope, from, Some(newWhere), orderBy)

  @targetName("sortByWithTuple")
  inline transparent def sortBy(fn: T => Tuple): QueryBuilder[T] =
    checkTupleOf[OrderBy](fn(scope))
    new QueryBuilder(scope, from, where, fn(scope).toList.asInstanceOf[List[OrderBy]])

  @targetName("sortByWithTuple1")
  inline transparent def sortBy(fn: T => OrderBy): QueryBuilder[T] =
    sortBy(s => Tuple1(fn(s)))

end QueryBuilder


type ColumnsToExpressions[T <: Tuple] <: Tuple =
  T match
    case EmptyTuple => EmptyTuple
    case Column[r, n] *: t => ColumnValue[r, n] *: ColumnsToExpressions[t]

def columnsToExpressions[T <: Tuple](columns: T, relation: Relation): ColumnsToExpressions[T] =
  columns match
    case _: EmptyTuple => EmptyTuple
    case t: (Column[r, n] *: tailType) =>
      ColumnValue[r, n](t.head.name, relation) *: columnsToExpressions(t.tail, relation)

transparent inline def from[T <: Tuple](table: Table[T]) =
  QueryBuilderFactory.create(
    columnsToExpressions(table.columns, table),
    table.tableName,
    None,
    List.empty,
  )
