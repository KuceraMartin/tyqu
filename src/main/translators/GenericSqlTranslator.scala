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

      case And(lhs, rhs) =>
        val tl = wrapInBraces[Or](lhs)
        val tr = wrapInBraces[And | Or](rhs)
        f"$tl AND $tr"

      case Or(lhs, rhs) =>
        val tl = wrapInBraces[And](lhs)
        val tr = wrapInBraces[And | Or](rhs)
        f"$tl OR $tr"

      case Not(expr) =>
        val tr = wrapInBraces[And | Or | Not](expr)
        f"NOT $tr"

      case CountAll() =>
        "COUNT(*)"

      case Plus(lhs, rhs) =>
        val tl = translateExpression(lhs)
        var tr = wrapInBraces[Plus[_] | Minus[_]](rhs)
        f"${tl} + ${tr}"

      case Minus(lhs, rhs) =>
        val tl = translateExpression(lhs)
        val tr = wrapInBraces[Plus[_] | Minus[_]](rhs)
        f"$tl - $tr"

      case Multiply(lhs, rhs) =>
        val tl = wrapInBraces[Plus[_] | Minus[_]](lhs)
        val tr = wrapInBraces[Plus[_] | Minus[_] | Multiply[_] | Divide[_]](rhs)
        f"$tl * $tr"

      case Divide(lhs, rhs) =>
        val tl = translateExpression(lhs)
        val tr = wrapInBraces[Plus[_] | Minus[_] | Multiply[_] | Divide[_]](rhs)
        f"$tl / $tr"

      case Function(name, List(arg1, arg2)) if (platform.isInfixOperator(name)) =>
        f"${translateExpression(arg1)} $name ${translateExpression(arg2)}"

      case Function(name, lst) =>
        f"$name(${lst.map(translateExpression).mkString(", ")})"


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
