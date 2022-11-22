package tyqu

import scala.quoted.*


class TableDefinitionException(message: String) extends Exception(message)


type ScopeSubtype[T <: TupleScope] = T
type StringSubtype[T <: String & Singleton] = T


object QueryBuilderFactory:

  transparent inline def fromObject[T <: TableRelation](inline table: T) =
    ${ fromObjectImpl[T]('table) }

  private def fromObjectImpl[T <: TableRelation: Type](table: Expr[T])(using Quotes) =
    import quotes.reflect.*

    val classSymbol = table.asTerm.underlying match
      case Apply(_, List(s)) => s.tpe.classSymbol.get

    val fields = classSymbol.declaredFields

    val (selection, refinementType) =
      fields.foldRight(('{EmptyTuple}: Expr[Tuple], TypeRepr.of[TupleScope])){ (field, acc) =>
        val (accSelection, accRefinement) = acc
        field.typeRef.translucentSuperType.asType match
          case '[Column[t]] =>
            val nameTp = ConstantType(StringConstant(field.name)).asType
            val nameExpr = Expr(field.name)
            val cv = nameTp match
              case '[StringSubtype[n]] =>
                '{ColumnValue[t, n](${nameExpr.asExprOf[n]}, $table)}
            val tp = Refinement(accRefinement, field.name, cv.asTerm.tpe)
            ('{$cv *: ${accSelection}}, tp)
          case _ =>
            val tableName = classSymbol.name.stripSuffix("$")
            val tp = field.typeRef.classSymbol.get.fullName
            throw TableDefinitionException(f"Table ${tableName} has property ${field.name} of type $tp which is not an allowed member of a table definition!")
      }

    refinementType.asType match
      case '[ScopeSubtype[t]] => '{ new QueryBuilder[t](TupleScope($selection, isSelectStar = true).asInstanceOf[t], $table)}


  transparent inline def fromTuple[T <: Tuple](inline selection: T, inline qb: QueryBuilder[_]): QueryBuilder[?] =
    ${fromTupleImpl[T]('selection, 'qb)}

  private def fromTupleImpl[T <: Tuple : Type](selection: Expr[T], qb: Expr[QueryBuilder[_]])(using Quotes): Expr[QueryBuilder[?]] =
    ScopeFactory.refine[T, TupleScope] match
      case '[ScopeSubtype[t]] => '{ $qb.copy(scope = TupleScope($selection)).asInstanceOf[QueryBuilder[t]] }
