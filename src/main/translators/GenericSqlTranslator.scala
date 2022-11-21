package tyqu.translators

import scala.reflect.TypeTest

import tyqu.platforms.Platform
import tyqu.*


class GenericSqlTranslator(platform: Platform):

  def translate(qb: QueryBuilder[_]): String =

    def collectRelations(
      exprs: List[Expression[_]],
      counter: Map[String, Int] = Map.empty,
      aliases: Map[TableRelation, String] = Map.empty,
    ): (Map[String, Int], Map[TableRelation, String]) =
      exprs.foldLeft((counter, aliases)){ (acc, e) =>
        val (counter, aliases) = acc

        def addRelation(relation: Relation) =
          aliases.get(relation) match
            case Some(alias) => (counter, aliases)
            case None =>
              val tableName = relation.table._name
              val cnt = counter.getOrElse(tableName, 0) + 1
              val alias = if (cnt == 1) tableName else f"${tableName}_$cnt"
              (counter + (tableName -> cnt), aliases + (relation -> alias))

        e match
          case ColumnValue(_, relation) => addRelation(relation)
          case SubqueryExpression(qb) => addRelation(qb.from)
          case Function(_, args) => collectRelations(args, counter, aliases)
          case p: Product =>
            val args = p.productIterator.collect{ case e: Expression[_] => e }.toList
            collectRelations(args, counter, aliases)
      }

    val (counter, aliases) = collectRelations(List(
      qb.scope match
        case t: TupleScope => t._toList
        case e: Expression[_] => List(e),

      qb.where.toList,

      qb.orderBy.map{
        case e: Expression[_] => e
        case Asc(e) => e
        case Desc(e) => e
      },
      
    ).flatten)

    doTranslate(
      qb,
      aliases.map{ (relation, alias) =>
        if relation.table._name == alias && counter(alias) > 1 then
          (relation, f"${alias}_1")
        else
          (relation, alias)
      },
    )


  def doTranslate(qb: QueryBuilder[_], tableAliases: Map[TableRelation, String], indent: Int = 0): String =

    def translateSelectSope(scope: Scope) =
      (scope match
        case expr: Expression[_] => translateSelectExpression(expr)
        case tuple: TupleScope =>
          if (tuple._isSelectStar) f"${platform.formatIdentifier(qb.from.table._name)}.*"
          else tuple._toList.map(translateSelectExpression).mkString(", ")
      )


    def translateSelectExpression(select: Expression[_]): String = select match
      case Alias(alias, expression) =>
        f"${translateExpression(expression)} AS ${platform.formatIdentifier(alias)}"
      case _ => translateExpression(select)


    def translateOrderByExpression(ord: OrderBy) = ord match
      case Asc(expr) => translateExpression(expr) + " ASC"
      case Desc(expr) => translateExpression(expr) + " DESC"
      case expr: Expression[?] => translateExpression(expr)


    def translateExpression(expr: Expression[?]): String = expr match
      case ColumnValue(name, relation) =>
        platform.formatIdentifier(tableAliases(relation)) + "." + platform.formatIdentifier(relation.table._getColumnName(name))

      case Alias(name, _) =>
        platform.formatIdentifier(name)
      
      case SubqueryExpression(qb: QueryBuilder[?]) =>
        f"(\n${doTranslate(qb, tableAliases, indent + 1)}\n)"

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

      Some(
        f"FROM ${platform.formatIdentifier(qb.from.table._name)}" + (
          if (qb.from.table._name == tableAliases(qb.from)) ""
          else " " + platform.formatIdentifier(tableAliases(qb.from))
        )
      ),

      qb.where.map("WHERE " + translateExpression(_)),

      if (qb.orderBy.isEmpty) None
      else Some("ORDER BY " + qb.orderBy.map(translateOrderByExpression).mkString(", ")),

      qb.limit.map("LIMIT " + _),

      if (qb.offset > 0) Some(f"OFFSET ${qb.offset}")
      else None,

    ).flatten.map("  " * indent + _).mkString("\n")
