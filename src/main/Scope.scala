package tyqu

class Scope(items: Tuple) extends Selectable:
  val toList = items.productIterator.toList.asInstanceOf[List[NamedExpression[?, ?]]]

  private val columns = toList.map{ expr => (expr.alias, expr) }.toMap

  def selectDynamic(name: String): Any = columns(name)
