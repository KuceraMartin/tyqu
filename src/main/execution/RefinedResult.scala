package tyqu.execution

import scala.annotation.tailrec
import scala.quoted.*

import tyqu.Column
import tyqu.Scope
import tyqu.TableScope


trait RefinedResult[S <: Scope]:
  type Refined


object RefinedResult:

  transparent inline given refinedResult[S <: Scope]: RefinedResult[S] = ${ refinedResultImpl[S] }

  private def refinedResultImpl[S <: Scope : Type](using Quotes): Expr[RefinedResult[S]] =
    refinedResultType[S] match
      case '[t] => '{ new RefinedResult[S] { type Refined = t } }


  def refinedResultType[S <: Scope : Type](using Quotes): Type[?] =
    import quotes.reflect.*

    val tableScope = Symbol.classSymbol("tyqu.TableScope")
    val tupleScope = Symbol.classSymbol("tyqu.TupleScope")
    val expression = Symbol.classSymbol("tyqu.Expression")

    val scopeType = TypeRepr.of[S]
    
    val res = 
      if scopeType.derivesFrom(tableScope) then
        val classSymbol = scopeType.baseType(tableScope).typeArgs(0).classSymbol.get
        val fields = classSymbol.declaredFields
        fields.foldLeft(TypeRepr.of[Result]){ (acc, field) =>
          val nameTp = ConstantType(StringConstant(field.name)).asType
          val nameExpr = Expr(field.name)
          field.typeRef.translucentSuperType.asType match
            case '[Column[t]] =>
              Refinement(acc, field.name, TypeRepr.of[t])
            case _ =>
              acc
        }
        
      else if scopeType.derivesFrom(tupleScope) then
        @tailrec
        def rec(tpe: TypeRepr, acc: TypeRepr): TypeRepr = 
          tpe match
            case Refinement(parent, name, exprType) =>
              val valType = exprType.baseType(expression).typeArgs(0)
              rec(parent, Refinement(acc, name, valType))
            case _ => acc
        rec(scopeType, TypeRepr.of[Result])

      else if scopeType.derivesFrom(expression) then
        scopeType.baseType(expression).typeArgs(0)

      else
        throw Exception(s"Unexpected scope type ${scopeType.show}")
      
      end if

    res.asType

end RefinedResult
