package tyqu

import scala.quoted.*


object ScopeFactory:

  transparent inline def append[T <: TupleScope](inline scope: T, inline expression: NamedExpression[_, _]) = ${appendImpl('scope, 'expression)}

  private def appendImpl[T <: TupleScope](scope: Expr[T], expression: Expr[NamedExpression[_, _]])(using q: Quotes, t: Type[T]) =
    import quotes.reflect.*

    val originalType = scope.asTerm.tpe.dealias.widen
    val expressionType = expression.asTerm.tpe.dealias.widen
    val name =
      val ConstantType(constant) = expressionType
                                .baseType(Symbol.classSymbol("tyqu.NamedExpression"))
                                .typeArgs(1): @unchecked
      constant.value.asInstanceOf[String]

    def replaceRefinement(original: TypeRepr, name: String, newValue: TypeRepr): (TypeRepr, Boolean) =
      original match
        case Refinement(obj, k, value) =>
          if (k == name) (Refinement(obj, k, newValue), true)
          else
            val (inner, didReplace) = replaceRefinement(obj, name, newValue)
            (Refinement(inner, k, value), didReplace)
        case _ => (original, false)

    val (refinementType, didReplace) = replaceRefinement(originalType, name, expressionType)
    val newItems =
      if (didReplace) '{ replace($scope._items, ${Expr(name)}, $expression) }
      else '{ $scope._items :* $expression }
    
    refinementType.asType match
      case '[ScopeSubtype[t]] => '{ TupleScope($newItems, isSelectStar = false).asInstanceOf[t] }


  transparent inline def prepend[T <: TupleScope](inline expression: NamedExpression[_, _], inline scope: T) = ${prependImpl('expression, 'scope)}

  private def prependImpl[T <: TupleScope](expression: Expr[NamedExpression[_, _]], scope: Expr[T])(using q: Quotes, t: Type[T]) =
    import quotes.reflect.*

    val originalType = scope.asTerm.tpe.dealias.widen
    val expressionType = expression.asTerm.tpe.dealias.widen
    val refinementType = expressionType match
      case AppliedType(_, List(_, ConstantType(name))) =>
        Refinement(originalType, name.value.asInstanceOf[String], expressionType)

    refinementType.asType match
      case '[ScopeSubtype[t]] => '{ TupleScope($expression *: $scope._items, isSelectStar = false).asInstanceOf[t] }


  transparent inline def concatRight[S <: TupleScope, T <: Tuple](inline scope: S, inline tuple: T) = ${concatRightImpl('scope, 'tuple)}

  private def concatRightImpl[S <: TupleScope, T <: Tuple](scope: Expr[S], tuple: Expr[T])(using q: Quotes, s: Type[S], t: Type[T]) =
    import quotes.reflect.*

    val refinementType = refine(scope, tuple)

    refinementType.asType match
      case '[ScopeSubtype[t]] => '{ TupleScope($scope._items ++ $tuple, isSelectStar = false).asInstanceOf[t] }


  transparent inline def concatLeft[T <: Tuple, S <: TupleScope](inline tuple: T, inline scope: S) = ${concatLeftImpl('tuple, 'scope)}

  private def concatLeftImpl[T <: Tuple, S <: TupleScope](tuple: Expr[T], scope: Expr[S])(using q: Quotes, s: Type[S], t: Type[T]) =
    import quotes.reflect.*

    val refinementType = refine(scope, tuple)

    refinementType.asType match
      case '[ScopeSubtype[t]] => '{ TupleScope($tuple ++ $scope._items, isSelectStar = false).asInstanceOf[t] }


  def refine[S <: TupleScope, T <: Tuple](scope: Expr[S], selection: Expr[T])(using q: Quotes) =
    import quotes.reflect.*

    def inner(t: TypeRepr, acc: TypeRepr): TypeRepr =
      t.widen match
        case AppliedType(_, lst) => lst match // _ = TypeRef(_, "*:")
          case List(TypeRef(_, _), ConstantType(name)) =>
            Refinement(acc, name.value.asInstanceOf[String], t)
          case List(head, tail) => head match
            case AppliedType(_, List(_, ConstantType(name))) =>
              inner(tail, Refinement(acc, name.value.asInstanceOf[String], head))
          case l: List[AppliedType] =>
            l.foldLeft(acc) { (acc2, t2) => inner(t2, acc2) }
        case TypeRef(_, _) | TermRef(_, _) => acc

    inner(selection.asTerm.tpe.dealias, scope.asTerm.tpe.dealias)


  def replace(columns: Tuple, name: String, replacement: Expression[_]): Tuple =
    columns match
      case EmptyTuple => EmptyTuple
      case (h: NamedExpression[_, _]) *: t =>
        (if (h.alias == name) replacement else h) *: replace(t, name, replacement)
