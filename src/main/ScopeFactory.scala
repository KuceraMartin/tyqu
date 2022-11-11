package tyqu

import scala.quoted.*


object ScopeFactory:

  transparent inline def append[T <: TupleScope](inline scope: T, inline expression: NamedExpression[_, _]) = ${appendImpl('scope, 'expression)}

  private def appendImpl[T <: TupleScope](scope: Expr[T], expression: Expr[NamedExpression[_, _]])(using q: Quotes, t: Type[T]) =
    import quotes.reflect.*

    val originalType = scope.asTerm.tpe.dealias.widen
    val expressionType = expression.asTerm.tpe.dealias.widen
    val refinementType = expressionType match
      case AppliedType(_, List(_, ConstantType(name))) =>
        Refinement(originalType, name.value.asInstanceOf[String], expressionType)
    
    refinementType.asType match
      case '[ScopeSubtype[t]] => '{ TupleScope($scope._items :* $expression, isSelectStar = false).asInstanceOf[t] }


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
