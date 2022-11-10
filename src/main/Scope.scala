package tyqu

type Scope = TupleScope | Expression[_]

class TupleScope(
  items: Tuple,
  isSelectStar: Boolean = false,
) extends Selectable:

  val _toList = items.productIterator.toList.asInstanceOf[List[NamedExpression[?, ?]]]

  private val columns = _toList.map{ expr => (expr.alias, expr) }.toMap

  def selectDynamic(name: String): Any = columns(name)

  def _isSelectStar = isSelectStar
