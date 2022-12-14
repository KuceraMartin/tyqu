package tyqu

import utils.checkTupleOf


def camelToSnakeCase(s: String): String =
  if (s.isBlank) ""
  else s.head.toLower.toString + s.drop(1).flatMap{ c =>
      if (c.isUpper) "_" + c.toLower
      else c.toString
    }


abstract class Table(
  translateIdentifier: String => String = camelToSnakeCase,
):
  val _name = translateIdentifier(getClass.getSimpleName.stripSuffix("$"))

  def _getColumnName(property: String) = translateIdentifier(property)

  lazy val _columns = _colToExpr.keys

  lazy val _pk = _columns.find(_.primary).get

  lazy val _colToExpr: Map[Column[?], TableRelation[this.type] => ColumnValue[?, ?]] =
    getClass.getDeclaredMethods.collect { m =>
      m.getReturnType.getName match
        case "tyqu.Column" =>
          val col = m.invoke(this).asInstanceOf[Column[?]]
          val expr = (rel: Relation) => createColumnValue(col, m.getName, rel)
          (col -> expr)
    }.toMap

  private def createColumnValue[ReadType](col: Column[ReadType], name: String, rel: Relation) =
    ColumnValue[ReadType, name.type](name, rel)


// , WriteType <: ReadType | Null
case class Column[ReadType](
  primary: Boolean = false,
):
  override def equals(x: Any): Boolean = x match
    case c: Column[ReadType] => eq(c)
    case _ => false

case class ManyToOne[T <: Table](target: T, through: Column[?])
case class OneToMany[T <: Table](sourceTable: T, sourceProp: ManyToOne[?])
