package tyqu

import scala.quoted.*


class TableDefinitionException(message: String) extends Exception(message)


type ScopeSubtype[T <: Scope] = T
type TupleScopeSubtype[T <: TupleScope] = T
type StringSubtype[T <: String & Singleton] = T


object QueryBuilderFactory:

  transparent inline def fromObject[T <: Table](inline table: T) = ${ fromObjectImpl('table) }

  private def fromObjectImpl[T <: Table](table: Expr[T])(using q: Quotes, t: Type[T]) =
    import quotes.reflect.*

    val classSymbol = table.asTerm.tpe.classSymbol.get
    val fields = classSymbol.declaredFields

    val (selection, refinementType) =
      fields.foldRight(('{EmptyTuple}: Expr[Tuple], TypeRepr.of[TupleScope])){ (field, acc) =>
        val (accSelection, accRefinement) = acc
        field.typeRef.translucentSuperType match
          case AppliedType(TypeRef(TermRef(_, "tyqu"), "Column"), tr :: _) =>
            val nameTp = ConstantType(StringConstant(field.name))
            val nameExpr = Expr(field.name)
            val cv = tr.asType match
              case '[t] => nameTp.asType match
                case '[StringSubtype[n]] =>
                  '{ColumnValue[t, n]($nameExpr.asInstanceOf[n], $table)}
            val tp = Refinement(accRefinement, field.name, cv.asTerm.tpe)
            ('{$cv *: ${accSelection}}, tp)
          case _ =>
            val tableName = classSymbol.name.stripSuffix("$")
            val tp = field.typeRef.classSymbol.get.fullName
            throw TableDefinitionException(f"Table ${tableName} has property ${field.name} of type $tp which is not an allowed member of a table definition!")
      }

    refinementType.asType match
      case '[TupleScopeSubtype[t]] => '{ new QueryBuilder(TupleScope($selection, isSelectStar = true), $table).asInstanceOf[QueryBuilder[t]]}


  transparent inline def fromTuple[T <: Tuple](inline selection: T, inline qb: QueryBuilder[_]) = ${fromTupleImpl('selection, 'qb)}

  private def fromTupleImpl[T <: Tuple](selection: Expr[T], qb: Expr[QueryBuilder[_]])(using q: Quotes, t: Type[T]) =
    import quotes.reflect.*

    val scope = '{TupleScope($selection)}
    val refinementType = ScopeFactory.refine(scope, selection)

    refinementType.asType match
      case '[TupleScopeSubtype[t]] => '{ $qb.copy(scope = $scope).asInstanceOf[QueryBuilder[t]]}


  transparent inline def fromFilter[F <: Expression[Boolean], S <: Scope](inline filter: F, inline qb: QueryBuilder[S]) = ${fromFilterImpl('filter, 'qb)}

  private def fromFilterImpl[F <: Expression[Boolean], S <: Scope](filter: Expr[F], qb: Expr[QueryBuilder[S]])(using q: Quotes, f: Type[F], s: Type[S]) =
    import quotes.reflect.*

    val scope = '{ $qb.scope }
    val where = '{ $qb.where.map(_ && $filter).getOrElse($filter) }

    if (scope.isExprOf[TupleScope])
      def processNotNullColumns(t: TypeRepr, acc: Set[String] = Set.empty): Set[String] =
        t match
          case AppliedType(TypeRef(TermRef(_, "scala"), "*:"), List(col, tuple)) =>
            col.widenTermRefByName match
              case ConstantType(colName) =>
                processNotNullColumns(tuple, acc + colName.value.asInstanceOf[String])
          case AppliedType(TypeRef(TermRef(_, "scala"), "Tuple1"), List(tr)) =>
            val ConstantType(name) = tr.widenTermRefByName: @unchecked
            acc + name.value.asInstanceOf[String]
          case TypeRef(TermRef(TermRef(_, "scala"), "Tuple$package"), "EmptyTuple") =>
            acc
          case _ =>
            acc

      val notNullCols = filter.asTerm.tpe.dealias.widen match
        case AndType(
            AppliedType(_, _),
            Refinement(_, "NotNullColumns", TypeBounds(lb, ub)),
          ) if lb == ub =>
            processNotNullColumns(lb)
        case _ => Set.empty
      
      def removeNull(t: TypeRepr): TypeRepr =
        t match
          case OrType(t1, t2) =>
            if (t1.typeSymbol.name == "Null") t2
            else if (t2.typeSymbol.name == "Null") t1
            else OrType(removeNull(t1), removeNull(t2))
          case _ => t
      
      def updateRefinement(scopeType: TypeRepr): TypeRepr =
        scopeType match
          case Refinement(rest, name, tp) =>
            val prop =
              if (notNullCols.contains(name))
                val e = tp.baseClasses.find(_.fullName == "tyqu.NamedExpression").get
                val AppliedType(tr, List(t, n)) = tp.baseType(Symbol.classSymbol("tyqu.NamedExpression")): @unchecked
                AppliedType(tr, List(removeNull(t), n))
              else tp
            Refinement(updateRefinement(rest), name, prop)
          case _ => scopeType

      val refinementType = updateRefinement(TypeRepr.of[S])

      refinementType.asType match
        case '[ScopeSubtype[t]] => '{ $qb.copy(where = Some($where)).asInstanceOf[QueryBuilder[t]]}

    else
      println(filter.asTerm.tpe.dealias.widen)
      println()
      qb
