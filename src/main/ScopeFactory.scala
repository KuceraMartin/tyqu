package tyqu

import scala.quoted.*
import scala.annotation.tailrec


trait RefinedScope[S <: Scope]:
    type Refined

object RefinedScope:
 transparent inline given refinedScope[T <: Table]: RefinedScope[TableScope[T]] = ${ refinedScopeImpl[T] }

  def refinedScopeImpl[T <: Table : Type](using Quotes) =
    import quotes.reflect.*

    val classSymbol = TypeRepr.of[T].classSymbol.get
    val fields = classSymbol.declaredFields
    val refinedType =
      fields.foldRight(TypeRepr.of[TableScope[T]]){ (field, acc) =>
        val nameTp = ConstantType(StringConstant(field.name)).asType
        val nameExpr = Expr(field.name)
        field.typeRef.translucentSuperType.asType match
          case '[Column[t]] =>
            nameTp match
              case '[StringSubtype[n]] =>
                Refinement(acc, field.name, TypeRepr.of[ColumnValue[t, n]])
          case '[OneToMany[t]] => 
            Refinement(acc, field.name, TypeRepr.of[QueryBuilder[TableScope[t]]])
          case '[ManyToMany[t]] =>
            Refinement(acc, field.name, TypeRepr.of[QueryBuilder[TableScope[t]]])
          case '[ManyToOne[t]] =>
            Refinement(acc, field.name, TypeRepr.of[TableScope[t]])
          case _ =>
            acc
      }
    refinedType.asType match
      case '[t] => '{ new RefinedScope[TableScope[T]] { type Refined = t } }

object ScopeFactory:

  transparent inline def concatRight[S <: TupleScope, T <: Tuple](inline scope: S, inline tuple: T) = ${concatRightImpl[S, T]('scope, 'tuple)}

  private def concatRightImpl[S <: TupleScope : Type, T <: Tuple : Type](scope: Expr[S], tuple: Expr[T])(using Quotes) =
    val newItems = '{ replaceOrAppend($scope._items, $tuple) }
    refine[T, S] match
      case '[ScopeSubtype[t]] => '{ TupleScope($newItems).asInstanceOf[t] }


  transparent inline def concatLeft[T <: Tuple, S <: TupleScope](inline tuple: T, inline scope: S) = ${concatLeftImpl[T, S]('tuple, 'scope)}

  private def concatLeftImpl[T <: Tuple : Type, S <: TupleScope : Type](tuple: Expr[T], scope: Expr[S])(using Quotes) =
    val newItems = '{ replaceOrPrepend($scope._items, $tuple) }
    refine[T, S] match
      case '[ScopeSubtype[t]] => '{ TupleScope($newItems).asInstanceOf[t] }


  def refine[T <: Tuple : Type, S <: TupleScope : Type](using Quotes): Type[?] =
    import quotes.reflect.*

    val emptyTuple = Symbol.classSymbol("scala.EmptyTuple")
    val cons = Symbol.classSymbol("scala.*:")

    def refineSingle(col: TypeRepr, base: TypeRepr) =
      val namedExpr = col.baseType(Symbol.classSymbol("tyqu.NamedExpression"))
      namedExpr match
        case AppliedType(_, List(_, ConstantType(StringConstant(name)))) =>
          Refinement(base, name, namedExpr)
        case _ =>
          throw new Exception(f"Unexpected type ${col.show}, expecting tyqu.NamedExpression")

    @tailrec
    def rec(tpe: TypeRepr, acc: TypeRepr): TypeRepr =
      tpe.widenTermRefByName.dealias match
        case AppliedType(fn, tpes) if defn.isTupleClass(fn.typeSymbol) =>
          tpes.foldRight(acc)(refineSingle)
        case AppliedType(tp, List(head, tail)) if tp.derivesFrom(cons) =>
          rec(tail, refineSingle(head, acc))
        case tpe if tpe.derivesFrom(emptyTuple) =>
          acc

    val res = rec(TypeRepr.of[T], TypeRepr.of[S])
    res.asType


  def replaceOrAppend(tuple: Tuple, replacements: Tuple): Tuple =
    foldLeft(replacements)(tuple){
      case (tuple: Tuple, e: NamedExpression[?, ?]) =>
        val (res, didReplace) = replace(tuple, e)
        if (didReplace) res
        else res :* e
    }
    
  def replaceOrPrepend(tuple: Tuple, replacements: Tuple): Tuple =
    foldRight(replacements)(tuple){
      case (e: NamedExpression[?, ?], acc: Tuple) =>
        val (res, didReplace) = replace(acc, e)
        if (didReplace) res
        else e *: res
    }

  def replace(tuple: Tuple, replacement: NamedExpression[?, ?]): (Tuple, Boolean) =
    foldLeft(tuple)((EmptyTuple: Tuple, false)){
      case ((acc, didReplace), e: NamedExpression[?, ?]) =>
        if e.alias == replacement.alias then
          (acc :* replacement, true)
        else
          (acc :* e, didReplace)
    }

  def foldLeft[B](tuple: Tuple)(z: B)(op: (B, Any) => B): B =
    tuple match
      case EmptyTuple => z
      case h *: t => foldLeft(t)(op(z, h))(op)

  def foldRight[B](tuple: Tuple)(z: B)(op: (Any, B) => B): B =
    tuple match
      case EmptyTuple => z
      case h *: t => op(h, foldRight(t)(z)(op))
