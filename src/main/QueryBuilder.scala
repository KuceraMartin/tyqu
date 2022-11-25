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

  @targetName("mapToScope")
  def map[T2 <: Scope](fn: T => T2): QueryBuilder[T2] =
    if isMapped then
      new QueryBuilder(fn(scope), SubqueryRelation(this), isMapped = true)
    else
      copy(scope = fn(scope), isMapped = true)
  
  @targetName("mapToTuple")
  inline transparent def map[T2 <: Tuple](inline fn: T => T2): QueryBuilder[?] =
    checkTupleOf[NamedExpression[_, _]](fn(scope))
    val qb =
      if isMapped then
        val newRelation = SubqueryRelation(this)
        scope match
          case ts: TupleScope =>
            new QueryBuilder(ts._replaceRelation(newRelation), newRelation)
          /*case e: NamedExpression[t, n] =>
            new QueryBuilder(ColumnValue[t, n](e.alias, newRelation), newRelation)
          case e: Expression[t] =>
            this*/
      else
        this
    QueryBuilderFactory.fromTuple(fn(scope), qb)

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


transparent inline def from[T <: Table](table: T) =
  val relation = TableRelation(table)
  QueryBuilderFactory.fromObject(relation)
