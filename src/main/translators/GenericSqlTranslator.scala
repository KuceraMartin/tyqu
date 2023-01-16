package tyqu.translators

import scala.language.unsafeNulls
import scala.reflect.TypeTest

import tyqu.platforms.Platform
import tyqu.*


class GenericSqlTranslator(platform: Platform):

  private def relationsFromExpression(e: Expression[?], withSubqueries: Boolean): Seq[Relation] =
    e match
      case ColumnValue(_, relation) => List(relation)
      case SubqueryExpression(qb) => if withSubqueries then collectRelations(qb) else Nil
      case Function(_, args) => args.flatMap(relationsFromExpression(_, withSubqueries))
      case p: Product =>
        p.productIterator.collect{ case e: Expression[?] => e }.flatMap(relationsFromExpression(_, withSubqueries)).toSeq

  private def collectRelations(qb: QueryBuilder[?], withSubqueries: Boolean = true): Seq[Relation] =
    List(
      qb.from match
        case SubqueryRelation(qb2) => if withSubqueries then collectRelations(qb2) else Nil
        case _: TableRelation[?] => Nil,

      List(qb.from),

      qb.scope match
        case t: TupleScope => t.toList.flatMap(relationsFromExpression(_, withSubqueries))
        case t: TableScope[?] => List(t.relation)
        case e: Expression[?] => relationsFromExpression(e, withSubqueries),

      relationsFromExpression(qb.where, withSubqueries),

      qb.orderBy.map{
          case e: Expression[?] => e
          case Asc(e) => e
          case Desc(e) => e
        }
        .flatMap(relationsFromExpression(_, withSubqueries)),
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


  private def doTranslate(
    qb: QueryBuilder[?],
    tableAliases: Map[Relation, String],
    inScope: Set[Relation] = Set.empty,
    indent: Boolean = true,
  ): String =
    val joinSucc = collectRelations(qb).flatMap{
        case r: JoinRelation[?] =>
          val e = r.on(r)
          relationsFromExpression(e, withSubqueries = true).map{ p => (p -> r) }
        case _ => Nil
      }
      .groupMap(_._1)(_._2)
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

    def translateFromRelation(relation: Relation, joins: Seq[JoinRelation[?]], complexFormatting: Boolean = false): String =
      val indent = if (complexFormatting) "    " else ""
      val f = relation match
        case r: TableRelation[?] =>
          translateFromTableClause(r)
        case SubqueryRelation(qb) =>
          f"(\n${doTranslate(qb, tableAliases, newInScope)}\n) ${platform.formatIdentifier(tableAliases(relation))}"
      (f +: joins.map(translateJoinRelation)).mkString(f"\n$indent")

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
          platform.formatStringLiteral(value.toString)

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

        case IsNull(expr) =>
          f"${translateExpression(expr)} IS NULL"

        case IsNotNull(expr) =>
          f"${translateExpression(expr)} IS NOT NULL"

        case Exists(subquery) =>
          f"EXISTS ${translateExpression(subquery)}"

        case StartsWith(needle, haystack) =>
          val likeExpression = f"$needle%%"
          f"${translateExpression(haystack)} LIKE ${platform.formatStringLiteral(likeExpression)}"

        case EndsWith(needle, haystack) =>
          val likeExpression = f"%%$needle"
          f"${translateExpression(haystack)} LIKE ${platform.formatStringLiteral(likeExpression)}"

        case Contains(needle, haystack) =>
          val likeExpression = f"%%$needle%%"
          f"${translateExpression(haystack)} LIKE ${platform.formatStringLiteral(likeExpression)}"

        case CountAll() =>
          "COUNT(*)"

        case Plus(lhs, rhs) =>
          val tl = translateExpression(lhs)
          var tr = wrapInParentheses[Plus[?, ?] | Minus[?, ?]](rhs)
          f"${tl} + ${tr}"

        case Minus(lhs, rhs) =>
          val tl = translateExpression(lhs)
          val tr = wrapInParentheses[Plus[?, ?] | Minus[?, ?]](rhs)
          f"$tl - $tr"

        case Multiply(lhs, rhs) =>
          val tl = wrapInParentheses[Plus[?, ?] | Minus[?, ?]](lhs)
          val tr = wrapInParentheses[Plus[?, ?] | Minus[?, ?] | Multiply[?, ?] | Divide[?, ?]](rhs)
          f"$tl * $tr"

        case Divide(lhs, rhs) =>
          val tl = translateExpression(lhs)
          val tr = wrapInParentheses[Plus[?, ?] | Minus[?, ?] | Multiply[?, ?] | Divide[?, ?]](rhs)
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


    val from = relations.collect{
        case r: (FromRelation[?] | SubqueryRelation) if !inScope.contains(r) =>
          (r, joinSucc.getOrElse(r, Seq.empty))
      }

    val res = List(
      Some("SELECT " + translateSelectSope(qb.scope)),

      Some(
        "FROM" + (
          if (from.size > 1 && from.exists(_._2.nonEmpty))
            f"\n  ${from.map{ case (r, j) => translateFromRelation(r, j, true) }.mkString(",\n  ")}"
          else
            f" ${from.map{ case (r, j) => translateFromRelation(r, j, false) }.mkString(", ")}"
        )
      ),

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
