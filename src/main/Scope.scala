package tyqu

class Scope(items: Tuple) extends Selectable:
  private val columns = items.productIterator.map {
      case expr: NamedExpression[?, ?] => (expr.alias, expr)
    }
    .toMap

  def selectDynamic(name: String): Any = columns(name)

  def toList = columns.values.toList
