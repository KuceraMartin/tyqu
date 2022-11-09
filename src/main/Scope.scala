package tyqu

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
