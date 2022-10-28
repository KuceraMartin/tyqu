package tyqu.translators

import tyqu.platforms.Platform
import tyqu.Alias
import tyqu.ColumnValue
import tyqu.Concat
import tyqu.Equal
import tyqu.Expression
import tyqu.NotEqual
import tyqu.QueryBuilder
import tyqu.LiteralExpression
import tyqu.Numeric

class GenericSqlTranslator(platform: Platform):

  def translate(qb: QueryBuilder[?]): String =
    List(
      "SELECT " + qb.scope.toList.map(translateSelectExpression).mkString(", "),
      "FROM " + platform.formatIdentifier(qb.from),
      if (qb.where.isEmpty) null
      else "WHERE " + qb.where.map(translateExpression).mkString(" AND ")
    ).filter(_ != null).mkString("\n")


  private def translateSelectExpression(select: Expression[?]): String =
    select match
      case Alias(alias, expression) =>
        f"${translateExpression(expression)} AS ${platform.formatIdentifier(alias)}"
      case _ => translateExpression(select)


  private def translateExpression(expr: Expression[?]): String = expr match
      case ColumnValue(_, name, relation) =>
        platform.formatIdentifier(relation) + "." + platform.formatIdentifier(name)

      case Alias(name, _) =>
        platform.formatIdentifier(name)

      case LiteralExpression(value: Numeric) =>
        value.toString

      case LiteralExpression(value) =>
        f"'${value.toString}'"

      case Equal(lhs, rhs) =>
        f"${translateExpression(lhs)} = ${translateExpression(rhs)}"

      case NotEqual(lhs, rhs) =>
        f"${translateExpression(lhs)} != ${translateExpression(rhs)}"

      case Concat(lhs, rhs) =>
        f"CONCAT(${translateExpression(lhs)}, ${translateExpression(rhs)})"
