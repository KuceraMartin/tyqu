package tyqu

import scala.quoted.*


class TableDefinitionException(message: String) extends Exception(message)


type ScopeSubtype[T <: TupleScope] = T
type StringSubtype[T <: String & Singleton] = T


object QueryBuilderFactory:

  transparent inline def fromObject[T <: TableRelation](inline table: T) = ${ fromObjectImpl('table) }

  private def fromObjectImpl[T <: TableRelation](table: Expr[T])(using q: Quotes, t: Type[T]) =
    import quotes.reflect.*

    val classSymbol = table.asTerm.underlying match
      case Apply(_, List(s)) => s.tpe.classSymbol.get

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
      case '[ScopeSubtype[t]] => '{ new QueryBuilder(TupleScope($selection, isSelectStar = true), $table).asInstanceOf[QueryBuilder[t]]}


  transparent inline def fromTuple[T <: Tuple](inline selection: T, inline qb: QueryBuilder[_]) = ${fromTupleImpl('selection, 'qb)}

  private def fromTupleImpl[T <: Tuple](selection: Expr[T], qb: Expr[QueryBuilder[_]])(using q: Quotes, t: Type[T]) =
    import quotes.reflect.*

    val scope = '{TupleScope($selection)}
    val refinementType = ScopeFactory.refine(scope, selection)

    refinementType.asType match
      case '[ScopeSubtype[t]] => '{ $qb.copy(scope = $scope).asInstanceOf[QueryBuilder[t]]}
