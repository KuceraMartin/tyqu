package tyqu

import utils.checkTupleOf


type Scope = TupleScope | Expression[_]


class TupleScope(
  val _items: Tuple,
  isSelectStar: Boolean = false,
) extends Selectable:

  def _toList = _items.toList.map{
    case (_, (gen: Function0[NamedExpression[_, _]])) => gen()
  }.asInstanceOf[List[NamedExpression[_, _]]]

  private lazy val cols = _items.toList.asInstanceOf[List[(String, () => Any)]].toMap

  def selectDynamic(name: String): Any = cols(name)()

  def _isSelectStar = isSelectStar

  transparent inline infix def :*(expr: NamedExpression[_, _]) =
    ScopeFactory.append(this, expr)

  // transparent inline infix def ++[T <: Tuple](tuple: T): TupleScope =
  //   checkTupleOf[NamedExpression[_, _]](tuple)
  //   ScopeFactory.concatRight(this, tuple)

end TupleScope


// extension (lhs: NamedExpression[_, _]) {
//   transparent inline infix def *:[S <: TupleScope](scope: S) =
//     ScopeFactory.prepend(lhs, scope)
// }


// extension [T <: Tuple](lhs: T) {
//   transparent inline infix def ++[S <: TupleScope](scope: S) =
//     checkTupleOf[NamedExpression[_, _]](lhs)
//     ScopeFactory.concatLeft(lhs, scope)
// }
