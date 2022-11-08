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
  def _getColumnName(property: String) = translateIdentifier(property)
  val _relationName = translateIdentifier(getClass.getSimpleName.stripSuffix("$"))


// , WriteType <: ReadType | Null
case class Column[ReadType](
  primary: Boolean = false,
)


abstract class Relationship
case class ManyToOne(target: Table) extends Relationship
