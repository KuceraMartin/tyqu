package tyqu

import scala.language.unsafeNulls


def camelToSnakeCase(s: String): String =
  if (s.isBlank) ""
  else s.head.toLower.toString + s.drop(1).flatMap{ c =>
      if (c.isUpper) "_" + c.toLower
      else c.toString
    }


abstract class Table(
  translateIdentifier: String => String = camelToSnakeCase,
):
  private[tyqu] val tableName = translateIdentifier(getClass.getSimpleName.stripSuffix("$"))

  private[tyqu] def getColumnName(property: String) = translateIdentifier(property)

  private lazy val columnsWithNames: Seq[(Column[?], String)] =
    getClass.getDeclaredMethods.collect{ m =>
      m.getReturnType.getName match
        case "tyqu.Column" if m.getParameterCount() == 0 =>
          val col = m.invoke(this).asInstanceOf[Column[?]]
          (col, m.getName)
    }

  private[tyqu] lazy val columns: Seq[Column[?]] =
    columnsWithNames.map(_._1)

  private[tyqu] lazy val colToName: Map[Column[?], String] =
    columnsWithNames.toMap

  private[tyqu] lazy val colToExpr: Map[Column[?], TableRelation[this.type] => ColumnValue[?, true, ?]] =
    colToName.map { (col, name) =>
      val expr = (rel: Relation) => createColumnValue(col, name, rel)
      (col -> expr)
    }

  private[tyqu] lazy val pk = columns.find(_.primary).get

  private def createColumnValue[ReadType](col: Column[ReadType], name: String, rel: Relation) =
    ColumnValue[ReadType, true, name.type](name, rel)


// , WriteType <: ReadType | Null
case class Column[ReadType](
  primary: Boolean = false,
):
  override def equals(x: Any): Boolean = x match
    case c: Column[ReadType] => eq(c)
    case _ => false

case class ManyToOne[T <: Table, Nullable <: Boolean](target: T, through: Column[?])
object ManyToOne:
  def apply[T <: Table](target: T, through: Column[?], nullable: Boolean = false): ManyToOne[T, nullable.type] =
    new ManyToOne[T, nullable.type](target, through)

case class OneToMany[T <: Table](sourceTable: T, sourceProp: ManyToOne[?, ?])
case class ManyToMany[T <: Table](target: T, joiningTable: Table, sourceColumn: Column[?], targetColumn: Column[?])
