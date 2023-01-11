package tyqu

import utils.checkTupleOrInstanceOf


case class QueryBuilder[T <: Scope](
  private[tyqu] scope: T,
  private[tyqu] from: Relation,
  private[tyqu] where: Expression[Boolean] = NoFilterExpression,
  private[tyqu] orderBy: List[OrderBy] = List.empty,
  private[tyqu] limit: Option[Int] = None,
  private[tyqu] offset: Int = 0,
):

  inline transparent def map[S <: Scope, T1 <: Tuple, T2 <: (S | T1)](using ref: RefinedScope[T])(inline fn: ref.Refined => T2): QueryBuilder[?] =
    val (originalScope, newQb) = prepareMap
    QueryBuilderFactory.fromMap[ref.Refined & Scope, S, T1, T2](originalScope.asInstanceOf[ref.Refined & Scope], newQb, fn)

  def filter(using ref: RefinedScope[T])(predicate: ref.Refined => Expression[Boolean]): QueryBuilder[T] =
    val expr = predicate(scope.asInstanceOf[ref.Refined])
    copy(where = where && expr)
  
  def exists(using ref: RefinedScope[T])(predicate: ref.Refined => Expression[Boolean]): Expression[Boolean] =
    Exists(SubqueryExpression(filter(predicate).map(_ => 1).asInstanceOf))

  def sortBy[Res <: Tuple | OrderBy](using ref: RefinedScope[T])(fn: ref.Refined => Res)(using checkTupleOrInstanceOf[Res, OrderBy] =:= true): QueryBuilder[T] =
    val newOrderBy = fn(scope.asInstanceOf[ref.Refined]) match
      case t: Tuple => t.toList.asInstanceOf[List[OrderBy]]
      case o: OrderBy => List(o)
    copy(orderBy = newOrderBy)

  def sorted(desc: Boolean): QueryBuilder[T] =
      // val newOrderBy = if desc then Desc(qb.scope) else Asc(qb.scope)
      val newOrderBy =
        (scope match
            case e: Expression[?] => List(e)
            case s: TupleScope => s.toList
            case s: TableScope[?] => s.toList
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

  private def prepareMap =
    if scope.isInstanceOf[TableScope[?]] then
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
      val newQb = new QueryBuilder(newScope, newRelation)
      (newScope, newQb)

end QueryBuilder


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
