package tyqu


def camelToSnakeCase(s: String) =
  if (s.isBlank) ""
  else s.head.toLower + s.drop(1).flatMap{ c =>
      if (c.isUpper) "_" + c.toLower
      else c.toString
    }
    .mkString


abstract class Relation(val name: String):
  def getColumnName(property: String) = property


case class Table[T <: Tuple](
  tableName: String,
  columns: T,
  namingConvention: String => String = camelToSnakeCase,
) extends Relation(tableName):
  override def getColumnName(property: String): String = namingConvention(property)


// , WriteType <: ReadType | Null
case class Column[ReadType, NameType <: String & Singleton](name: NameType, primary: Boolean = false)


def column[T](name: String) = Column[T, name.type](name)
