package tyqu

case class Table[T <: Tuple](
  val tableName: String,
  val columns: T,
)


// , WriteType <: ReadType | Null
case class Column[ReadType, AliasType <: String & Singleton](alias: AliasType, name: String, primary: Boolean = false)

def column[T](alias: String, name: String): Column[T, alias.type] =
  Column[T, alias.type](alias, name)
def column[T](alias: String): Column[T, alias.type] = column[T](alias, alias)
