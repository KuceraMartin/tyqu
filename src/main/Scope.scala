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
