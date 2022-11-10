package tyqu.translators

import scala.reflect.TypeTest

import tyqu.platforms.Platform
import tyqu.*


class GenericSqlTranslator(platform: Platform):

  def translate(qb: QueryBuilder[?], indent: Int = 0): String =

    def translateSelectSope(scope: Scope) =
      (scope match
        case expr: Expression[?] => translateSelectExpression(expr)
        case tuple: TupleScope =>
          if (tuple._isSelectStar) f"${platform.formatIdentifier(qb.from._relationName)}.*"
          else tuple._toList.map(translateSelectExpression).mkString(", ")
      )


    def translateSelectExpression(select: Expression[?]): String = select match
      case Alias(alias, expression) =>
        f"${translateExpression(expression)} AS ${platform.formatIdentifier(alias)}"
      case _ => translateExpression(select)


    def translateOrderByExpression(ord: OrderBy) = ord match
      case Asc(expr) => translateExpression(expr) + " ASC"
      case Desc(expr) => translateExpression(expr) + " DESC"
      case expr: Expression[?] => translateExpression(expr)


    def translateExpression(expr: Expression[?]): String = expr match
      case ColumnValue(name, relation) =>
        platform.formatIdentifier(relation._relationName) + "." + platform.formatIdentifier(relation._getColumnName(name))

      case Alias(name, _) =>
        platform.formatIdentifier(name)
      
      case SubqueryExpression(qb: QueryBuilder[?]) =>
        f"(\n${translate(qb, indent + 1)}\n)"

      case LiteralExpression(value: Numeric) =>
        value.toString

      case LiteralExpression(value) =>
        f"'${value.toString}'"

      case LessThan(lhs, rhs) =>
        f"${translateExpression(lhs)} < ${translateExpression(rhs)}"

      case GreaterThan(lhs, rhs) =>
        f"${translateExpression(lhs)} > ${translateExpression(rhs)}"

      case Equal(lhs, rhs) =>
        f"${translateExpression(lhs)} = ${translateExpression(rhs)}"

      case NotEqual(lhs, rhs) =>
        f"${translateExpression(lhs)} != ${translateExpression(rhs)}"

      case And(lhs, rhs) =>
        val List(tl, tr) = List(lhs, rhs).map(wrapInBraces[Or])
        f"$tl AND $tr"

      case Or(lhs, rhs) =>
        val List(tl, tr) = List(lhs, rhs).map(wrapInBraces[And])
        f"$tl OR $tr"

      case Not(expr) =>
        val tr = wrapInBraces[And | Or](expr)
        f"NOT $tr"

      case CountAll() =>
        "COUNT(*)"

      case Count(expr) =>
        f"COUNT(${translateExpression(expr)})"

      case Min(expr) =>
        f"MIN(${translateExpression(expr)})"

      case Max(expr) =>
        f"Max(${translateExpression(expr)})"

      case Average(expr) =>
        f"AVG(${translateExpression(expr)})"

      case Sum(expr) =>
        f"SUM(${translateExpression(expr)})"

      case Plus(lhs, rhs) =>
        f"${translateExpression(lhs)} + ${translateExpression(rhs)}"

      case Minus(lhs, rhs) =>
        val tl = translateExpression(lhs)
        val tr = wrapInBraces[Plus[_] | Minus[_]](rhs)
        f"$tl - $tr"

      case Multiply(lhs, rhs) =>
        val List(tl, tr) = List(lhs, rhs).map(wrapInBraces[Plus[_] | Minus[_]])
        f"$tl * $tr"

      case Divide(lhs, rhs) =>
        val tl = translateExpression(lhs)
        val tr = wrapInBraces[Plus[_] | Minus[_] | Multiply[_] | Divide[_]](rhs)
        f"$tl / $tr"

      case Concat(lhs, rhs) =>
        f"CONCAT(${translateExpression(lhs)}, ${translateExpression(rhs)})"


    def wrapInBraces[T](e: Expression[_])(using TypeTest[Expression[_], T]): String =
      val translated = translateExpression(e)
      e match
        case _: T => f"($translated)"
        case _ => translated

    List(
      Some("SELECT " + translateSelectSope(qb.scope)),

      Some("FROM " + platform.formatIdentifier(qb.from._relationName)),

      qb.where.map("WHERE " + translateExpression(_)),

      if (qb.orderBy.isEmpty) None
      else Some("ORDER BY " + qb.orderBy.map(translateOrderByExpression).mkString(", ")),

      qb.limit.map("LIMIT " + _),

      if (qb.offset > 0) Some(f"OFFSET ${qb.offset}")
      else None,

    ).flatten.map("  " * indent + _).mkString("\n")
