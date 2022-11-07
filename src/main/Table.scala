package tyqu

import utils.checkTupleOf


def camelToSnakeCase(s: String) =
  if (s.isBlank) ""
  else s.head.toLower + s.drop(1).flatMap{ c =>
      if (c.isUpper) "_" + c.toLower
      else c.toString
    }
    .mkString


abstract class Relation(
  val _relationName: String,
  val _getColumnName: String => String = identity,
)

abstract class Table(
  name: String,
  propertyToColumnName: String => String = camelToSnakeCase,
) extends Relation(name, propertyToColumnName)

// , WriteType <: ReadType | Null
case class Column[ReadType](name: String, primary: Boolean = false)


def column[T](name: String) = Column[T](name)
