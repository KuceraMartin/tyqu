package tyqu

import scala.quoted.*


type ScopeSubtype[T <: TupleScope] = T
type StringSubtype[T <: String & Singleton] = T


object QueryBuilderFactory:

  transparent inline def fromMap[T1 <: Scope, S <: Scope, T <: Tuple, T2 <: (S | T)](inline originalScope: T1, inline newQb: QueryBuilder[?], inline fn: T1 => T2): QueryBuilder[?] =
    ${fromMapImpl[T1, S, T, T2]('originalScope, 'newQb, 'fn)}

  private def fromMapImpl[T1 <: Scope : Type, S <: Scope : Type, T <: Tuple : Type, T2 <: S | T : Type](originalScope: Expr[T1], newQb: Expr[QueryBuilder[?]], fn: Expr[T1 => T2])(using Quotes): Expr[QueryBuilder[?]] =
    import quotes.reflect.*

    val function1 = Symbol.classSymbol("scala.Function1")
    val tuple = Symbol.classSymbol("scala.Tuple")

    val AppliedType(_, List(_, resType)) = fn.asTerm.tpe.baseType(function1)
    if (resType.derivesFrom(tuple))
      val selection = '{$fn($originalScope)}.asExprOf[T]
      ScopeFactory.refine[T, TupleScope] match
        case '[ScopeSubtype[t]] =>
          '{ $newQb.copy(scope = TupleScope($selection)).asInstanceOf[QueryBuilder[t]] }
    else
      '{
        val newScope = $fn($originalScope).asInstanceOf[S]
        $newQb.copy(scope = newScope.asInstanceOf[QueryBuilder.WidenScopeType[S]])
      }
