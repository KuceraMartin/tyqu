package tyqu

import scala.quoted.*
import scala.annotation.tailrec


object ScopeFactory:

  transparent inline def append[T <: TupleScope, E <: NamedExpression[?, ?]](inline scope: T, inline expression: E) =
    ${appendImpl[T, E]('scope, 'expression)}

  private def appendImpl[T <: TupleScope : Type, E <: NamedExpression[?, ?] : Type](scope: Expr[T], expression: Expr[E])(using Quotes) =
    refine[E, T] match
      case '[ScopeSubtype[t]] => '{ TupleScope($scope._items :* $expression, isSelectStar = false).asInstanceOf[t] }


  transparent inline def prepend[E <: NamedExpression[?, ?], T <: TupleScope](inline expression: E, inline scope: T) = ${prependImpl[E, T]('expression, 'scope)}

  private def prependImpl[E <: NamedExpression[?, ?] : Type, T <: TupleScope : Type](expression: Expr[E], scope: Expr[T])(using Quotes) =
    refine[E, T] match
      case '[ScopeSubtype[t]] => '{ TupleScope($expression *: $scope._items, isSelectStar = false).asInstanceOf[t] }


  transparent inline def concatRight[S <: TupleScope, T <: Tuple](inline scope: S, inline tuple: T) = ${concatRightImpl[S, T]('scope, 'tuple)}

  private def concatRightImpl[S <: TupleScope : Type, T <: Tuple : Type](scope: Expr[S], tuple: Expr[T])(using Quotes) =
    refine[T, S] match
      case '[ScopeSubtype[t]] => '{ TupleScope($scope._items ++ $tuple, isSelectStar = false).asInstanceOf[t] }


  transparent inline def concatLeft[T <: Tuple, S <: TupleScope](inline tuple: T, inline scope: S) = ${concatLeftImpl[T, S]('tuple, 'scope)}

  private def concatLeftImpl[T <: Tuple : Type, S <: TupleScope : Type](tuple: Expr[T], scope: Expr[S])(using Quotes) =
    refine[T, S] match
      case '[ScopeSubtype[t]] => '{ TupleScope($tuple ++ $scope._items, isSelectStar = false).asInstanceOf[t] }


  def refine[T : Type, S <: TupleScope : Type](using Quotes): Type[?] =
    import quotes.reflect.*

    val emptyTuple = Symbol.classSymbol("scala.EmptyTuple")
    val cons = Symbol.classSymbol("scala.*:")

    def refineSingle(col: TypeRepr, base: TypeRepr) =
      col.baseType(Symbol.classSymbol("tyqu.NamedExpression")) match
        case AppliedType(_, List(_, ConstantType(StringConstant(name)))) =>
          Refinement(base, name, col)

    @tailrec
    def rec(tpe: TypeRepr, acc: TypeRepr): TypeRepr =
      tpe.widenTermRefByName.dealias match
        case AppliedType(fn, tpes) if defn.isTupleClass(fn.typeSymbol) =>
          tpes.foldRight(acc)(refineSingle)
        case AppliedType(tp, List(head, tail)) if tp.derivesFrom(cons) =>
          rec(tail, refineSingle(head, acc))
        case tpe if tpe.derivesFrom(emptyTuple) =>
          acc
        case _ =>
          refineSingle(tpe, acc)

    rec(TypeRepr.of[T], TypeRepr.of[S]).asType
