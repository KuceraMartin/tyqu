package tyqu

import scala.quoted.*


class TableDefinitionException(message: String) extends Exception(message)


type ScopeSubtype[T <: TupleScope] = T
type StringSubtype[T <: String & Singleton] = T


object QueryBuilderFactory:

  transparent inline def fromObject[T <: TableRelation[?]](inline table: T) =
    ${ fromObjectImpl[T]('table) }

  private def fromObjectImpl[T <: TableRelation[?]: Type](table: Expr[T])(using Quotes) =
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
            acc
      }

    refinementType.asType match
      case '[ScopeSubtype[t]] => '{ new QueryBuilder[t](TupleScope($selection, isSelectStar = true).asInstanceOf[t], $table)}


  transparent inline def fromMap[T1 <: Scope, S <: Scope, T <: Tuple, T2 <: (S | T)](inline originalScope: T1, inline newQb: QueryBuilder[?], inline fn: T1 => T2): QueryBuilder[?] =
    ${fromMapImpl[T1, S, T, T2]('originalScope, 'newQb, 'fn)}

  private def fromMapImpl[T1 <: Scope : Type, S <: Scope : Type, T <: Tuple : Type, T2 <: S | T : Type](originalScope: Expr[T1], newQb: Expr[QueryBuilder[?]], fn: Expr[T1 => T2])(using Quotes): Expr[QueryBuilder[?]] =
    import quotes.reflect.*

    val function1 = Symbol.classSymbol("scala.Function1")
    val tuple = Symbol.classSymbol("scala.Tuple")

    val AppliedType(_, List(_, resType)) = fn.asTerm.tpe.baseType(function1)
    if (resType.derivesFrom(tuple))
      val fnRes = '{$fn($originalScope)}.asExprOf[T]
      fromTupleImpl[T]('{$fn($originalScope)}.asExprOf[T], newQb)
    else
      '{
        val newScope = $fn($originalScope).asInstanceOf[S]
        $newQb.copy(scope = newScope.asInstanceOf[QueryBuilder.WidenScopeType[newScope.type]], isMapped = true)
      }    


  transparent inline def fromTuple[T <: Tuple](inline selection: T, inline qb: QueryBuilder[_]): QueryBuilder[?] =
    ${fromTupleImpl[T]('selection, 'qb)}

  private def fromTupleImpl[T <: Tuple : Type](selection: Expr[T], qb: Expr[QueryBuilder[_]])(using Quotes): Expr[QueryBuilder[?]] =
    ScopeFactory.refine[T, TupleScope] match
      case '[ScopeSubtype[t]] => '{ $qb.copy(scope = TupleScope($selection), isMapped = true).asInstanceOf[QueryBuilder[t]] }
