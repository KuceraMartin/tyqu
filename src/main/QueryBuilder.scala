package tyqu

import utils.checkTupleOrInstanceOf


case class QueryBuilder[T <: Scope](
  private[tyqu] scope: T,
  private[tyqu] from: Relation,
  private[tyqu] isMapped: Boolean = false,
  private[tyqu] where: Option[Expression[Boolean]] = None,
  private[tyqu] orderBy: List[OrderBy] = List.empty,
  private[tyqu] limit: Option[Int] = None,
  private[tyqu] offset: Int = 0,
):

  inline transparent def map[S <: Scope, T1 <: Tuple, T2 <: (S | T1)](using ref: RefinedScope[T])(inline fn: ref.Refined => T2): QueryBuilder[?] =
    val (originalScope, newQb) = prepareMap
    QueryBuilderFactory.fromMap[ref.Refined & Scope, S, T1, T2](originalScope.asInstanceOf[ref.Refined & Scope], newQb, fn)

  def filter(using ref: RefinedScope[T])(predicate: ref.Refined => Expression[Boolean]): QueryBuilder[T] =
    val expr = predicate(scope.asInstanceOf[ref.Refined])
    copy(where = Some(where.map(_ && expr).getOrElse(expr)))
  
  def exists(using ref: RefinedScope[T])(predicate: ref.Refined => Expression[Boolean]): Expression[Boolean] =
    Exists(SubqueryExpression(filter(predicate).map(_ => 1).asInstanceOf))

  def sortBy[Res <: Tuple | OrderBy](using ref: RefinedScope[T])(fn: ref.Refined => Res)(using checkTupleOrInstanceOf[Res, OrderBy] =:= true): QueryBuilder[T] =
    val newOrderBy = fn(scope.asInstanceOf[ref.Refined]) match
      case t: Tuple => t.toList.asInstanceOf[List[OrderBy]]
      case o: OrderBy => List(o)
    copy(orderBy = newOrderBy)

  def limitBy(limit: Int, offset: Int = 0): QueryBuilder[T] =
    copy(limit = Some(limit), offset = offset)

  def count = copy(scope = CountAll(), isMapped = true)

  private def prepareMap =
    if isMapped then
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
    else
      (scope, this)

end QueryBuilder


object QueryBuilder:

  type WidenScopeType[T <: Scope] <: Scope = T match
    case NamedExpression[t, n] => NamedExpression[t, n]
    case Expression[t] => Expression[t]
    case _ => T

  extension [E <: Expression[?]](qb: QueryBuilder[E])
    def sorted(desc: Boolean): QueryBuilder[E] =
      val newOrderBy = if desc then Desc(qb.scope) else Asc(qb.scope)
      qb.copy(orderBy = List(newOrderBy))

    def sorted: QueryBuilder[E] = sorted(desc = false)

end QueryBuilder


def from[T <: Table](table: T) =
  val rel = FromRelation(table)
  val scope = TableScope(rel)
  QueryBuilder(scope, rel)
