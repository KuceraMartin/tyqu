package tyqu

import scala.quoted.*
import scala.annotation.tailrec
import org.checkerframework.checker.units.qual.m


trait RefinedScope[S <: Scope]:
  type Refined

trait GroupMapScope[G <: Tuple | Scope, S <: Scope]:
  type Refined <: Scope

object RefinedScope:
  transparent inline given refinedScope[T <: Table, Nullable <: Boolean]: RefinedScope[TableScope[T, Nullable]] = ${ refinedScopeImpl[T, Nullable] }

  private def refinedScopeImpl[T <: Table : Type, Nullable <: Boolean : Type](using Quotes): Expr[RefinedScope[TableScope[T, Nullable]]] =
    import quotes.reflect.*

    val isNullable = TypeRepr.of[Nullable] =:= TypeRepr.of[true]

    generateRefinedTableScope(isNullable) match
      case '[t] => '{ new RefinedScope[TableScope[T, Nullable]] { type Refined = t } }

  transparent inline given groupMapScope[G <: Tuple | Scope, S <: Scope]: GroupMapScope[G, S] = ${ groupMapScopeImpl[G, S] }

  private def groupMapScopeImpl[G <: Tuple | Scope : Type, S <: Scope : Type](using Quotes) =
    import quotes.reflect.*

    val emptyTuple = Symbol.classSymbol("scala.EmptyTuple")
    val cons = Symbol.classSymbol("scala.*:")
    val tuple = Symbol.classSymbol("scala.Tuple")
    val namedExpression = Symbol.classSymbol("tyqu.NamedExpression")
    val tupleScope = Symbol.classSymbol("tyqu.TupleScope")
    val tableScope = Symbol.classSymbol("tyqu.TableScope")

    val gType = TypeRepr.of[G]
    val sType = TypeRepr.of[S]

    val groupedBy: Set[String] =
      if gType.derivesFrom(tuple) then
        @tailrec
        def rec(tpe: TypeRepr, acc: Set[String]): Set[String] =
          tpe.widenTermRefByName.dealias match
            case AppliedType(fn, tpes) if defn.isTupleClass(fn.typeSymbol) =>
              acc ++ tpes.map{ _.baseType(namedExpression) match
                case AppliedType(_, List(_, _, ConstantType(StringConstant(name)))) =>
                  name
              }
            case AppliedType(tp, List(head, tail)) if tp.derivesFrom(cons) =>
              rec(tail, acc)
            case tpe if tpe.derivesFrom(emptyTuple) =>
              acc
        rec(gType, Set.empty)
      else if gType.derivesFrom(namedExpression) then
        val AppliedType(_, List(_, _, ConstantType(StringConstant(name)))) = gType.baseType(namedExpression)
        Set(name)
      else
        Set.empty

    val updatedScope =
      if sType.derivesFrom(tupleScope) then
        def rec(tpe: TypeRepr): TypeRepr =
          tpe.widenTermRefByName.dealias match
            case Refinement(base, name, tp) =>
              val newTp =
                if groupedBy.contains(name) then
                  tp
                else
                  tp.baseType(namedExpression) match
                    case AppliedType(_, List(tr, _, name)) =>
                      tr.asType match
                        case '[t] =>
                          name.asType match
                            case '[StringSubtype[n]] =>
                              TypeRepr.of[NamedExpression[t, false, n]]
              Refinement(rec(base), name, newTp)
            case tupleScope => tupleScope
            case _ =>
              throw Exception(s"Unexpected type ${tpe}")
        rec(sType)
      else if sType.derivesFrom(tableScope) then
        type BooleanSubtype[T <: Boolean] = T
        val AppliedType(_, List(tableType, _)) = sType.baseType(tableScope)

        type TableSubtype[T <: Table] = T

        val classSymbol = tableType.classSymbol.get
        val baseTableScope =
          tableType.asType match
            case '[TableSubtype[t]] => TypeRepr.of[TableScope[t, false]]

        val fields = classSymbol.declaredFields
        fields.foldRight(baseTableScope){ (field, acc) =>
          val nameTp = ConstantType(StringConstant(field.name)).asType
          val nameExpr = Expr(field.name)
          val canSelect =
            if groupedBy.contains(field.name) then
              TypeRepr.of[true]
            else TypeRepr.of[false]
          field.typeRef.translucentSuperType.asType match
            case '[Column[t]] =>
              (nameTp, canSelect.asType) match
                case ('[StringSubtype[n]], '[BooleanSubtype[canSelect]]) =>
                  Refinement(acc, field.name, TypeRepr.of[ColumnValue[t, canSelect, n]])
            case _ =>
              acc
          }
      else if sType.derivesFrom(namedExpression) then
        val AppliedType(_, List(tr, _, nameTr @ ConstantType(StringConstant(name)))) = sType.baseType(namedExpression)
        if groupedBy.contains(name) then
          sType
        else
          (tr.asType, nameTr.asType) match
            case ('[t], '[StringSubtype[n]]) =>
              TypeRepr.of[NamedExpression[t, false, n]]
      else
        throw Exception(s"Unexpected scope type ${sType.show}")

    type GenericScopeSubtype[T <: Scope] = T

    updatedScope.asType match
      case '[GenericScopeSubtype[t]] => '{ new GroupMapScope[G, S] { type Refined = t } }


  private def generateRefinedTableScope[T <: Table : Type](isNullable: Boolean, groupBy: Option[Set[String]] = None)(using Quotes): Type[?] =
    import quotes.reflect.*

    type BooleanSubtype[T <: Boolean] = T

    val classSymbol = TypeRepr.of[T].classSymbol.get
    val baseTableScope =
      if isNullable then
        TypeRepr.of[TableScope[T, true]]
      else
        TypeRepr.of[TableScope[T, false]]

    val fields = classSymbol.declaredFields
    val refinedType =
      fields.foldRight(baseTableScope){ (field, acc) =>
        val nameTp = ConstantType(StringConstant(field.name)).asType
        val nameExpr = Expr(field.name)
        val canSelect =
          groupBy match
            case Some(s) if s.contains(field.name) => TypeRepr.of[true]
            case None => TypeRepr.of[true]
            case _ => TypeRepr.of[false]
        field.typeRef.translucentSuperType.asType match
          case '[Column[t]] =>
            (nameTp, canSelect.asType) match
              case ('[StringSubtype[n]], '[BooleanSubtype[canSelect]]) =>
                if isNullable then
                  Refinement(acc, field.name, TypeRepr.of[ColumnValue[t | Null, canSelect, n]])
                else
                  Refinement(acc, field.name, TypeRepr.of[ColumnValue[t, canSelect, n]])
          case _ if groupBy.nonEmpty =>
            acc
          case '[OneToMany[t]] => 
            Refinement(acc, field.name, TypeRepr.of[QueryBuilder[TableScope[t, false]]])
          case '[ManyToMany[t]] =>
            Refinement(acc, field.name, TypeRepr.of[QueryBuilder[TableScope[t, false]]])
          case '[ManyToOne[t, n]] =>
            Refinement(acc, field.name, TypeRepr.of[TableScope[t, n]])
          case _ =>
            acc
      }

    refinedType.asType


object ScopeFactory:

  transparent inline def concatRight[S <: TupleScope, T <: Tuple](inline scope: S, inline tuple: T) = ${concatRightImpl[S, T]('scope, 'tuple)}

  private def concatRightImpl[S <: TupleScope : Type, T <: Tuple : Type](scope: Expr[S], tuple: Expr[T])(using Quotes) =
    val newItems = '{ replaceOrAppend($scope.items, $tuple) }
    refine[T, S] match
      case '[ScopeSubtype[t]] => '{ TupleScope($newItems).asInstanceOf[t] }


  transparent inline def concatLeft[T <: Tuple, S <: TupleScope](inline tuple: T, inline scope: S) = ${concatLeftImpl[T, S]('tuple, 'scope)}

  private def concatLeftImpl[T <: Tuple : Type, S <: TupleScope : Type](tuple: Expr[T], scope: Expr[S])(using Quotes) =
    val newItems = '{ replaceOrPrepend($scope.items, $tuple) }
    refine[T, S] match
      case '[ScopeSubtype[t]] => '{ TupleScope($newItems).asInstanceOf[t] }


  def refine[T <: Tuple : Type, S <: TupleScope : Type](using Quotes): Type[?] =
    import quotes.reflect.*

    val emptyTuple = Symbol.classSymbol("scala.EmptyTuple")
    val cons = Symbol.classSymbol("scala.*:")

    def refineSingle(col: TypeRepr, base: TypeRepr) =
      val namedExpr = col.baseType(Symbol.classSymbol("tyqu.NamedExpression"))
      namedExpr match
        case AppliedType(_, List(_, _, ConstantType(StringConstant(name)))) =>
          Refinement(base, name, namedExpr)
        case _ =>
          println(namedExpr)
          throw new Exception(f"... $namedExpr ... Unexpected type ${col.show}, expecting tyqu.NamedExpression")

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
      case (tuple: Tuple, e: NamedExpression[?, ?, ?]) =>
        val (res, didReplace) = replace(tuple, e)
        if (didReplace) res
        else res :* e
    }
    
  def replaceOrPrepend(tuple: Tuple, replacements: Tuple): Tuple =
    foldRight(replacements)(tuple){
      case (e: NamedExpression[?, ?, ?], acc: Tuple) =>
        val (res, didReplace) = replace(acc, e)
        if (didReplace) res
        else e *: res
    }

  def replace(tuple: Tuple, replacement: NamedExpression[?, ?, ?]): (Tuple, Boolean) =
    foldLeft(tuple)((EmptyTuple: Tuple, false)){
      case ((acc, didReplace), e: NamedExpression[?, ?, ?]) =>
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
