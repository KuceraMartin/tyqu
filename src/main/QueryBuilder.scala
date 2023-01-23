package tyqu

import execution.QueryExecutor
import utils.checkTupleOrInstanceOf


case class QueryBuilder[T <: Scope](
  private[tyqu] scope: T,
  private[tyqu] from: FromRelation[?] | SubqueryRelation,
  private[tyqu] where: Expression[Boolean] = NoFilterExpression,
  private[tyqu] groupBy: List [Expression[?]] = List.empty,
  private[tyqu] orderBy: List[OrderBy] = List.empty,
  private[tyqu] limit: Option[Int] = None,
  private[tyqu] offset: Int = 0,
):

  inline transparent def map[S <: Scope, T1 <: Tuple, T2 <: (S | T1)](inline fn: T => T2): QueryBuilder[?] =
    val (originalScope, newQb) = prepareMap
    QueryBuilderFactory.fromMap[T, S, T1, T2](originalScope, newQb, fn)

  def flatMap[S2 <: Scope](using ref: RefinedScope[T])(fn: ref.Refined => QueryBuilder[S2]): QueryBuilder[S2] =
    val (originalScope, newQb) = prepareMap
    val qb2 = fn(originalScope.asInstanceOf[ref.Refined & Scope])
    QueryBuilder(
      qb2.scope,
      from,
      where = where && qb2.where,
      orderBy = orderBy ++ qb2.orderBy,
    )

  def filter(using ref: RefinedScope[T])(predicate: ref.Refined => Expression[Boolean]): QueryBuilder[T] =
    val expr = predicate(scope.asInstanceOf[ref.Refined])
    copy(where = where && expr)

  inline transparent def groupMap[G <: (Tuple | Scope), S <: Scope, T1 <: Tuple, M <: (S | T1)](using ref: RefinedScope[T])(g: ref.Refined => G)(m: T => M): QueryBuilder[?] =
    val groupBy: List[Expression[?]] = inline g(scope.asInstanceOf) match
      case t: Tuple => t.toList.asInstanceOf[List[Expression[?]]]
      case s: MultiScope => s.toList
      case e: Expression[?] => List(e)
    val qb2 = copy(groupBy = groupBy)
    val (originalScope, qb3) = qb2.prepareMap
    QueryBuilderFactory.fromMap[T, S, T1, M](originalScope, qb3, m)
  
  def exists(using ref: RefinedScope[T])(predicate: ref.Refined => Expression[Boolean]): Expression[Boolean] =
    Exists(SubqueryExpression(filter(predicate).map(_ => LiteralExpression(1, static = true)).asInstanceOf))

  def sortBy[Res <: Tuple | OrderBy](using ref: RefinedScope[T])(fn: ref.Refined => Res)(using checkTupleOrInstanceOf[Res, OrderBy] =:= true): QueryBuilder[T] =
    val newOrderBy = fn(scope.asInstanceOf[ref.Refined]) match
      case t: Tuple => t.toList.asInstanceOf[List[OrderBy]]
      case o: OrderBy => List(o)
    copy(orderBy = newOrderBy)

  def sorted(desc: Boolean): QueryBuilder[T] =
      val newOrderBy =
        (scope match
            case e: Expression[?] => List(e)
            case s: TupleScope => s.toList
            case s: TableScope[?, ?] => s.toList
        ).map(e => if desc then e.desc else e.asc)
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
          case e: Expression[t] => e.as("v")
          case s => s
      val newRelation = SubqueryRelation(copy(scope = originalScope))
      val newScope = (originalScope match
          case ts: TupleScope => ts.replaceRelation(newRelation)
          case e: NamedExpression[t, n] => ColumnValue[t, n](e.alias, newRelation)
        ).asInstanceOf[T]
      val newQb = QueryBuilder(scope = newScope, from = newRelation)
      (newScope, newQb)

end QueryBuilder


extension [T <: Numeric | Null, E <: Expression[T]] (qb: QueryBuilder[E])
  def sum =
    qb.copy(scope = Function[T | Null]("SUM", List(qb.scope)))

  def min =
    qb.copy(scope = Function[T | Null]("MIN", List(qb.scope)))

  def max =
    qb.copy(scope = Function[T | Null]("MAX", List(qb.scope)))

  def avg =
    qb.copy(scope = Function[T | Null]("AVG", List(qb.scope)))


object QueryBuilder:

  type WidenScopeType[T <: Scope] <: Scope = T match
    case NamedExpression[t, n] => NamedExpression[t, n]
    case Expression[t] => Expression[t]
    case _ => T

end QueryBuilder


def from[T <: Table](table: T) =
  val rel = FromRelation(table)
  val scope = TableScope(rel)
  QueryBuilder(scope, rel)
