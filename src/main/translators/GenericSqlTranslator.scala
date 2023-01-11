package tyqu.translators

import scala.reflect.TypeTest

import tyqu.platforms.Platform
import tyqu.*


class GenericSqlTranslator(platform: Platform):

  private def collectRelations(qb: QueryBuilder[?], withSubqueries: Boolean = true): Seq[Relation] =
    def fromExpression(e: Expression[?]): Seq[Relation] =
      e match
        case ColumnValue(_, relation) => List(relation)
        case SubqueryExpression(qb) => if withSubqueries then collectRelations(qb) else Nil
        case Function(_, args) => args.flatMap(fromExpression)
        case p: Product =>
          p.productIterator.collect{ case e: Expression[?] => e }.flatMap(fromExpression).toSeq

    List(
      qb.from match
        case SubqueryRelation(qb2) => if withSubqueries then collectRelations(qb2) else Nil
        case _: TableRelation[?] => Nil,

      List(qb.from),

      qb.scope match
        case t: TupleScope => t.toList.flatMap(fromExpression)
        case t: TableScope[?] => List(t.relation)
        case e: Expression[?] => fromExpression(e),

      fromExpression(qb.where),

      qb.orderBy.map{
          case e: Expression[?] => e
          case Asc(e) => e
          case Desc(e) => e
        }
        .flatMap(fromExpression),
    ).flatten


  def translate(qb: QueryBuilder[?]): String =
    val (counter, aliases) = collectRelations(qb).foldLeft((Map[String, Int](), Map[Relation, String]())){ (acc, relation) =>
      val (counter, aliases) = acc
      aliases.get(relation) match
        case Some(alias) => (counter, aliases)
        case None =>
          val tableName = relation.underlyingName
          val cnt = counter.getOrElse(tableName, 0) + 1
          val alias = if (cnt == 1) tableName else f"${tableName}_$cnt"
          (counter + (tableName -> cnt), aliases + (relation -> alias))
    }

    doTranslate(
      qb,
      aliases.map{ (relation, alias) =>
        if relation.underlyingName == alias && counter(alias) > 1 then
            (relation, f"${alias}_1")
        else
          (relation, alias)
      },
      indent = false
    )


  private def doTranslate(qb: QueryBuilder[?], tableAliases: Map[Relation, String], inScope: Set[Relation] = Set.empty, indent: Boolean = true): String =
    val relations = collectRelations(qb, withSubqueries = false).toSet
    val newInScope = inScope ++ relations

    def translateSelectSope(scope: Scope) =
      scope match
        case expr: Expression[?] =>
          translateSelectExpression(expr)
        case scope: TupleScope =>
          scope.toList.map(translateSelectExpression).mkString(", ")
        case scope: TableScope[?] =>
          f"${platform.formatIdentifier(tableAliases(scope.relation))}.*"


    def translateSelectExpression(select: Expression[?]): String = select match
      case Alias(alias, expression) =>
        f"${translateExpression(expression)} AS ${platform.formatIdentifier(alias)}"
      case _ => translateExpression(select)


    def translateFromTableClause(relation: Relation): String =
      platform.formatIdentifier(relation.underlyingName) + (
          if (relation.underlyingName == tableAliases(relation)) ""
          else " " + platform.formatIdentifier(tableAliases(relation))
        )

    def translateFromRelation(relation: Relation): String = relation match
      case r: TableRelation[?] =>
        translateFromTableClause(r)
      case SubqueryRelation(qb) =>
        f"(\n${doTranslate(qb, tableAliases, newInScope)}\n) ${platform.formatIdentifier(tableAliases(relation))}"


    def translateJoinRelation(join: JoinRelation[?]): String =
      val joinClause = join.joinType match
        case JoinType.Inner => "JOIN"
        case JoinType.Left => "LEFT JOIN"
        case JoinType.Right => "RIGHT JOIN"
        case JoinType.FullOuter => "FULL OUTER JOIN"

      f"$joinClause ${translateFromTableClause(join)} ON ${translateExpression(join.on(join))}"


    def translateOrderByExpression(ord: OrderBy) = ord match
      case Asc(expr) => translateExpression(expr) + " ASC"
      case Desc(expr) => translateExpression(expr) + " DESC"
      case expr: Expression[?] => translateExpression(expr)


    def translateExpression(expr: Expression[?], inParentheses: Boolean = false): String =
      expr match
        case ColumnValue(name, relation) =>
          platform.formatIdentifier(tableAliases(relation)) + "." + platform.formatIdentifier(relation.getColumnName(name))

        case Alias(name, _) =>
          platform.formatIdentifier(name)

        case SubqueryExpression(qb: QueryBuilder[?]) =>
          val (lb, rb) = if (inParentheses) ("", "") else ("(", ")")
          f"$lb\n${doTranslate(qb, tableAliases, newInScope)}\n$rb"

        case LiteralExpression(value: Numeric) =>
          value.toString

        case LiteralExpression(value) =>
          f"'${value.toString}'"

        case And(lhs, rhs) =>
          val tl = wrapInParentheses[Or](lhs)
          val tr = wrapInParentheses[And | Or](rhs)
          f"$tl AND $tr"

        case Or(lhs, rhs) =>
          val tl = wrapInParentheses[And](lhs)
          val tr = wrapInParentheses[And | Or](rhs)
          f"$tl OR $tr"

        case Not(expr) =>
          val tr = wrapInParentheses[And | Or | Not](expr)
          f"NOT $tr"

        case Exists(subquery) =>
          f"EXISTS ${translateExpression(subquery)}"

        case CountAll() =>
          "COUNT(*)"

        case Plus(lhs, rhs) =>
          val tl = translateExpression(lhs)
          var tr = wrapInParentheses[Plus[?] | Minus[?]](rhs)
          f"${tl} + ${tr}"

        case Minus(lhs, rhs) =>
          val tl = translateExpression(lhs)
          val tr = wrapInParentheses[Plus[?] | Minus[?]](rhs)
          f"$tl - $tr"

        case Multiply(lhs, rhs) =>
          val tl = wrapInParentheses[Plus[?] | Minus[?]](lhs)
          val tr = wrapInParentheses[Plus[?] | Minus[?] | Multiply[?] | Divide[?]](rhs)
          f"$tl * $tr"

        case Divide(lhs, rhs) =>
          val tl = translateExpression(lhs)
          val tr = wrapInParentheses[Plus[?] | Minus[?] | Multiply[?] | Divide[?]](rhs)
          f"$tl / $tr"

        case Function(name, List(arg1)) =>
          f"$name(${translateExpression(arg1, inParentheses = true)})"

        case Function(name, List(arg1, arg2)) if (platform.isInfixOperator(name)) =>
          f"${translateExpression(arg1)} $name ${translateExpression(arg2)}"

        case Function(name, lst) =>
          f"$name(${lst.map(translateExpression(_)).mkString(", ")})"


    def wrapInParentheses[T](e: Expression[?])(using TypeTest[Expression[?], T]): String =
      val translated = translateExpression(e)
      e match
        case _: T => f"($translated)"
        case _ => translated


    val (from, join) =
      relations
        .filterNot(inScope.contains)
        .foldLeft((List[Relation](), List[JoinRelation[?]]())) { (acc, rel) =>
          val (from, join) = acc
          rel match
            case r: (FromRelation[?] | SubqueryRelation) => (r :: from, join)
            case r: JoinRelation[?] => (from, r :: join)
        }

    val res = List(
      Some("SELECT " + translateSelectSope(qb.scope)),

      Some(
        f"FROM ${from.map(translateFromRelation).mkString(", ")}"
      ),

      join.map(translateJoinRelation),

      qb.where match
        case NoFilterExpression => None
        case expr => Some("WHERE " + translateExpression(expr)),

      if (qb.orderBy.isEmpty) None
      else Some("ORDER BY " + qb.orderBy.map(translateOrderByExpression).mkString(", ")),

      qb.limit.map("LIMIT " + _),

      if (qb.offset > 0) Some(f"OFFSET ${qb.offset}")
      else None,

    ).flatten.mkString("\n")

    if indent then
      res.split("\n").map("  " + _).mkString("\n")
    else
      res
