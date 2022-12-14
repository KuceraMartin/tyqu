package tyqu

import scala.annotation.targetName

import utils.checkTupleOf


case class QueryBuilder[+T <: Scope](
  scope: T,
  from: Relation,
  isMapped: Boolean = false,
  where: Option[Expression[Boolean]] = None,
  orderBy: List[OrderBy] = List.empty,
  limit: Option[Int] = None,
  offset: Int = 0,
):

  type WidenScopeType[T <: Scope] <: Scope = T match
    case Expression[t] => Expression[t]
    case _ => T

  @targetName("mapToScope")
  def map[T2 <: Scope](fn: T => T2): QueryBuilder[WidenScopeType[T2]] =
    val (originalScope, newQb) = prepareMap
    val newScope = fn(originalScope).asInstanceOf[WidenScopeType[T2]]
    newQb.copy(scope = newScope, isMapped = true)
  
  @targetName("mapToTuple")
  inline transparent def map[T2 <: Tuple](inline fn: T => T2): QueryBuilder[?] =
    val (originalScope, newQb) = prepareMap
    val newScope = fn(originalScope)
    checkTupleOf[NamedExpression[_, _]](newScope)
    QueryBuilderFactory.fromTuple(newScope, newQb)

  private def prepareMap =
    if isMapped then
      // if we have only one value in scope -> name it so that it can be accessed
      val originalScope = scope match
          case e: Expression[t] => e.as("v")
          case s => s
      val newRelation = SubqueryRelation(copy(scope = originalScope))
      val newScope = (originalScope match
          case ts: TupleScope => ts._replaceRelation(newRelation)
          case e: NamedExpression[t, n] => ColumnValue[t, n](e.alias, newRelation)
        ).asInstanceOf[T]
      val newQb = new QueryBuilder(newScope, newRelation)
      (newScope, newQb)
    else
      (scope, this)

  def filter(predicate: T => Expression[Boolean]): QueryBuilder[T] =
    val expr = predicate(scope)
    copy(where = Some(where.map(_ && expr).getOrElse(expr)))

  @targetName("sortByWithTuple")
  inline transparent def sortBy(fn: T => Tuple): QueryBuilder[T] =
    checkTupleOf[OrderBy](fn(scope))
    copy(orderBy = fn(scope).toList.asInstanceOf[List[OrderBy]])

  @targetName("sortByWithTuple1")
  inline transparent def sortBy(fn: T => OrderBy): QueryBuilder[T] =
    sortBy(s => Tuple1(fn(s)))

  def limitBy(limit: Int, offset: Int = 0): QueryBuilder[T] =
    copy(limit = Some(limit), offset = offset)

  def count = map{ _ => CountAll() }

end QueryBuilder


def from[T <: Table](table: T) =
  val rel = FromRelation(table)
  // val scope = ScopeFactory.fromTable(rel)
  val scope = TableScope(rel)
  QueryBuilder(scope, rel)
