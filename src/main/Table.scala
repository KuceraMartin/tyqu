package tyqu

import utils.checkTupleOf


def camelToSnakeCase(s: String) =
  if (s.isBlank) ""
  else s.head.toLower + s.drop(1).flatMap{ c =>
      if (c.isUpper) "_" + c.toLower
      else c.toString
    }
    .mkString


abstract class Table(
  translateIdentifier: String => String = camelToSnakeCase,
):
  val _name = translateIdentifier(getClass.getSimpleName.stripSuffix("$"))
  def _getColumnName(property: String) = translateIdentifier(property)


// , WriteType <: ReadType | Null
case class Column[ReadType](
  primary: Boolean = false,
)
