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


// , WriteType <: ReadType | Null
case class Column[ReadType](
  primary: Boolean = false,
)
