package tyqu

import execution.QueryExecutor
import utils.checkTupleOrInstanceOf
import QueryBuilder.IsValidMapResult
import scala.annotation.targetName


case class QueryBuilder[T <: Scope](
  private[tyqu] scope: T,
  private[tyqu] from: FromRelation[?] | SubqueryRelation,
  private[tyqu] where: Expression[Boolean, true] = NoFilterExpression,
  private[tyqu] groupBy: List[AnyExpression] = List.empty,
  private[tyqu] having: Expression[Boolean, true] = NoFilterExpression,
  private[tyqu] orderBy: List[OrderBy] = List.empty,
  private[tyqu] limit: Option[Int] = None,
  private[tyqu] offset: Int = 0,
):

  inline transparent def map[S <: Scope, T1 <: Tuple, T2 <: (S | T1)](inline fn: T => T2)(using IsValidMapResult[T2] =:= true): QueryBuilder[?] =
    val (originalScope, newQb) = prepareMap
    QueryBuilderFactory.fromMap[T, S, T1, T2](originalScope, newQb, fn)

  def flatMap[S2 <: Scope](fn: T => QueryBuilder[S2])(using IsValidMapResult[S2] =:= true): QueryBuilder[S2] =
    val (originalScope, newQb) = prepareMap
    val qb2 = fn(originalScope)
    QueryBuilder(
      qb2.scope,
      from,
      where = where && qb2.where,
      orderBy = orderBy ++ qb2.orderBy,
    )

  def filter(predicate: T => Expression[Boolean, true]): QueryBuilder[T] =
    val expr = predicate(scope)
    if groupBy.isEmpty then
      copy(where = where && expr)
    else
      copy(having = having && expr)


  def withFilter(predicate: T => Expression[Boolean, true]): QueryBuilder[T] =
    filter(predicate)

  inline transparent def groupMap
    [G <: (Tuple | Scope), S <: Scope, T1 <: Tuple, M <: (S | T1)]
    (g: T => G)
    (using mapRef: GroupMapScope[G, T])
    (m: mapRef.Refined => M)
    (using IsValidMapResult[M] =:= true): QueryBuilder[?] =
      val (originalScope, qb2) = prepareMap
      val groupBy: List[Expression[?, true]] = inline g(originalScope.asInstanceOf) match
        case t: Tuple => t.toList.asInstanceOf[List[Expression[?, true]]]
        case s: MultiScope => s.toList
        case e: Expression[?, true] => List(e)
      QueryBuilderFactory
        .fromMap[mapRef.Refined, S, T1, M](originalScope.asInstanceOf[mapRef.Refined], qb2.copy(groupBy = groupBy), m)
  end groupMap
  
  def exists(predicate: T => Expression[Boolean, true]): Expression[Boolean, true] =
    Exists(SubqueryExpression(filter(predicate).map(_ => LiteralExpression(1, static = true)).asInstanceOf))

  def sortBy[Res <: Tuple | OrderBy](fn: T => Res)(using checkTupleOrInstanceOf[Res, OrderBy] =:= true): QueryBuilder[T] =
    val newOrderBy = fn(scope) match
      case t: Tuple => t.toList.asInstanceOf[List[OrderBy]]
      case o: OrderBy => List(o)
    copy(orderBy = newOrderBy)

  def sorted(desc: Boolean): QueryBuilder[T] =
      val newOrderBy =
        (scope match
            case e: Expression[?, true] => List(e)
            case s: TupleScope => s.toList
            case s: TableScope[?, ?] => s.toList
        ).map(e => if desc then Desc(e) else Asc(e))
      copy(orderBy = newOrderBy)

  def sorted: QueryBuilder[T] =
    sorted(desc = false)

  def limit(v: Int): QueryBuilder[T] =
    copy(limit = Some(v))

  def offset(v: Int): QueryBuilder[T] =
    copy(offset = v)

  def count =
    copy(scope = CountAll())

  transparent inline def execute()(using executor: QueryExecutor) =
    executor.execute(this)

  private def prepareMap =
    if scope.isInstanceOf[TableScope[?, ?]] then
      (scope, this)
    else
      // if we have only one value in scope -> name it so that it can be accessed
      val originalScope = scope match
          case e: Expression[t, true] => e.as("v")
          case s => s
      val newRelation = SubqueryRelation(copy(scope = originalScope))
      val newScope = (originalScope match
          case ts: TupleScope => ts.replaceRelation(newRelation)
          case e: NamedExpression[t, true, n] => ColumnValue[t, true, n](e.alias, newRelation)
        ).asInstanceOf[T]
      val newQb = QueryBuilder(scope = newScope, from = newRelation)
      (newScope, newQb)

end QueryBuilder


object QueryBuilder:

  type IsValidMapResult[R <: Tuple | Scope] <: Boolean = R match
    case Tuple => ArgsCanSelect[R]
    case Expression[?, ?] => ArgsCanSelect[Tuple1[R]]
    case _ => true

  // because of double map which is transformed to a subquery -> all expressions become ColumnValue but QueryBuilder[ColumnValue] is not a subtype of QueryBuilder[Expression]
  type WidenScopeType[T <: Scope] <: Scope = T match
    case NamedExpression[t, true, n] => NamedExpression[t, true, n]
    case Expression[t, true] => Expression[t, true]
    case _ => T

end QueryBuilder


def from[T <: Table](table: T) =
  val rel = FromRelation(table)
  val scope = TableScope(rel)
  QueryBuilder(scope, rel)
