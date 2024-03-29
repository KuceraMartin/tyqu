package tyqu

import scala.language.unsafeNulls

import utils.checkTupleOf


type Scope = MultiScope | Expression[?, true]

given [S <: TupleScope]: (RefinedScope[S] { type Refined = S }) = new RefinedScope[S] { type Refined = S }
given [T <: NamedExpression[?, true, ?]]: (RefinedScope[T] { type Refined = T } ) = new RefinedScope[T] { type Refined = T }
given [T, E <: Expression[T, true]]: (RefinedScope[E] { type Refined = Expression[T, ?] } ) = new RefinedScope[E] { type Refined = Expression[T, ?] }


abstract class MultiScope:
  private[tyqu] def toList: List[NamedExpression[?, true, ?]]


class TupleScope(private[tyqu] val items: Tuple) extends MultiScope with Selectable:

  private[tyqu] val toList = items.productIterator.toList.asInstanceOf[List[NamedExpression[?, true, ?]]]

  protected val columns = toList.map{ expr => (expr.alias, expr) }.toMap

  def selectDynamic(name: String): Any = columns(name)

  private[tyqu] def replaceRelation(relation: Relation): this.type =
    def rec(items: Tuple): Tuple =
      items match
        case EmptyTuple => EmptyTuple
        case (head: NamedExpression[t, s, n]) *: tail =>
          ColumnValue[t, s, n](head.alias, relation) *: rec(tail)
    new TupleScope(rec(items)).asInstanceOf[this.type]

end TupleScope


extension [S <: TupleScope](lhs: S) {
  transparent inline infix def :*[E <: NamedExpression[?, ?, ?]](inline expr: E) =
    lhs ++ Tuple1[E](expr)

  transparent inline infix def ++[T <: Tuple](inline tuple: T): TupleScope =
    checkTupleOf[NamedExpression[?, ?, ?]](tuple)
    ScopeFactory.concatRight(lhs, tuple)
}


extension [E <: NamedExpression[?, ?, ?]](lhs: E) {
  transparent inline infix def *:[S <: TupleScope](inline scope: S) =
    Tuple1(lhs) ++ scope
}


extension [T <: Tuple](lhs: T) {
  transparent inline infix def ++[S <: TupleScope](inline scope: S) =
    checkTupleOf[NamedExpression[?, ?, ?]](lhs)
    ScopeFactory.concatLeft(lhs, scope)
}


class TableScope[T <: Table, Nullable <: Boolean](
  private[tyqu] val relation: TableRelation[T],
) extends MultiScope with Selectable:

  private[tyqu] def pk = relation.pk

  private [tyqu] def toList: List[ColumnValue[?, true, ?]] =
    relation.table.columns.map(relation.colToExpr).toList

  def selectDynamic(name: String): Any =
    relation.table.getClass.getMethod(name).invoke(relation.table) match
      case c: Column[?] =>
        relation.colToExpr(c)
      case OneToMany(sourceTable, ManyToOne(_, through)) =>
        val propName = sourceTable.colToName(through)
        val qb = from(sourceTable)
        val expr = qb.scope.selectDynamic(propName).asInstanceOf[Expression[Any, true]] === pk.asInstanceOf[Expression[Any, true]]
        qb.copy(where = expr)
      case ManyToMany(targetTable, joiningTable, sourceColumn, targetColumn) =>
        val qb = from(targetTable)
        val targetScope = qb.scope
        val targetPk = targetScope.pk.asInstanceOf[Expression[Any, true]]
        val rel = JoinRelation(joiningTable, JoinType.Inner, { join =>
              join.colToExpr(targetColumn).asInstanceOf[Expression[Any, true]] === targetPk
            })
        val expr = rel.colToExpr(sourceColumn).asInstanceOf[Expression[Any, true]] === pk.asInstanceOf[Expression[Any, true]]
        qb.copy(where = expr)

      case ManyToOne(target, through) =>
        val throughExpr = relation.colToExpr(through).asInstanceOf[Expression[Any, true]]
        val rel = JoinRelation(target, JoinType.Left, { join =>
          join.pk.asInstanceOf[Expression[Any, true]] === throughExpr
        })
        TableScope(rel)
      case v => throw Exception(f"$name -> $v")

end TableScope

object TableScope:
  given [T <: Table, Nullable <: Boolean](using ref: RefinedScope[TableScope[T, Nullable]]): Conversion[TableScope[T, Nullable], ref.Refined] = _.asInstanceOf[ref.Refined]
