package tyqu

import utils.checkTupleOf


type Scope = TupleScope | Expression[_]


class TupleScope(
  val _items: Tuple,
  isSelectStar: Boolean = false,
) extends Selectable:

  val _toList = _items.productIterator.toList.asInstanceOf[List[NamedExpression[?, ?]]]

  private val columns = _toList.map{ expr => (expr.alias, expr) }.toMap

  def selectDynamic(name: String): Any = columns(name)

  def _isSelectStar = isSelectStar

  transparent inline infix def :*(expr: NamedExpression[_, _]) =
    ScopeFactory.append(this, expr)

  transparent inline infix def ++[T <: Tuple](tuple: T): TupleScope =
    checkTupleOf[NamedExpression[_, _]](tuple)
    ScopeFactory.concatRight(this, tuple)

end TupleScope


extension (lhs: NamedExpression[_, _]) {
  transparent inline infix def *:[S <: TupleScope](scope: S) =
    ScopeFactory.prepend(lhs, scope)
}
