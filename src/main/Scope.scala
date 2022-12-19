package tyqu

import java.lang.reflect.Modifier

import scala.reflect.ClassTag

import utils.checkTupleOf


type Scope = TupleScope | TableScope[_] | Expression[_]


class TupleScope(
  val _items: Tuple,
  isSelectStar: Boolean = false,
) extends Selectable:

  val _toList = _items.productIterator.toList.asInstanceOf[List[NamedExpression[?, ?]]]

  protected val columns = _toList.map{ expr => (expr.alias, expr) }.toMap

  def selectDynamic(name: String): Any = columns(name)

  def _isSelectStar = isSelectStar

  def _replaceRelation(relation: Relation): this.type =
    def rec(items: Tuple): Tuple =
      items match
        case EmptyTuple => EmptyTuple
        case (head: NamedExpression[t, n]) *: tail =>
          ColumnValue[t, n](head.alias, relation) *: rec(tail)
    new TupleScope(rec(_items), isSelectStar = true).asInstanceOf[this.type]

end TupleScope


extension [S <: TupleScope](lhs: S) {
  transparent inline infix def :*[E <: NamedExpression[?, ?]](inline expr: E) =
    lhs ++ Tuple1[E](expr)

  transparent inline infix def ++[T <: Tuple](inline tuple: T): TupleScope =
    checkTupleOf[NamedExpression[_, _]](tuple)
    ScopeFactory.concatRight(lhs, tuple)
}


extension [E <: NamedExpression[?, ?]](lhs: E) {
  transparent inline infix def *:[S <: TupleScope](inline scope: S) =
    Tuple1(lhs) ++ scope
}


extension [T <: Tuple](lhs: T) {
  transparent inline infix def ++[S <: TupleScope](inline scope: S) =
    checkTupleOf[NamedExpression[_, _]](lhs)
    ScopeFactory.concatLeft(lhs, scope)
}


class TableScope[T <: Table](
  val _relation: TableRelation[T],
  // val _items: Tuple,
  // relations: String => Table,
) extends Selectable:

  def _pk = _relation.pk

  def selectDynamic(name: String): Any =
    _relation.table.getClass.getMethod(name).invoke(_relation.table) match
      case c: Column[_] =>
        _relation.colToExpr(c)
      case OneToMany(sourceTable, ManyToOne(_, through)) =>
        val propName = sourceTable._colToName(through)
        from(sourceTable).filter(_.selectDynamic(propName).asInstanceOf[Expression[Any]] === _pk.asInstanceOf[Expression[Any]])
      case ManyToMany(targetTable, joiningTable, sourceColumn, targetColumn) =>
        from(targetTable)
          .filter{ targetScope =>
            val targetPk = targetScope._pk.asInstanceOf[Expression[Any]]
            val rel = JoinRelation(joiningTable, JoinType.Inner, { join =>
              join.colToExpr(targetColumn).asInstanceOf[Expression[Any]] === targetPk
            })
            rel.colToExpr(sourceColumn).asInstanceOf[Expression[Any]] === _pk.asInstanceOf[Expression[Any]]
          }

      case ManyToOne(target, through) =>
        val throughExpr = _relation.colToExpr(through).asInstanceOf[Expression[Any]]
        val rel = JoinRelation(target, JoinType.Inner, { join =>
          throughExpr === join.pk.asInstanceOf[Expression[Any]]
        })
        TableScope(rel)
      case v => throw Exception(f"$name -> $v")

end TableScope

object TableScope:
  given [T <: Table](using ref: RefinedScope[T]): Conversion[TableScope[T], ref.Refined] = _.asInstanceOf[ref.Refined]
