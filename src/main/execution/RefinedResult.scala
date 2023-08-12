package tyqu.execution

import scala.annotation.tailrec
import scala.quoted.*

import tyqu.Column
import tyqu.Scope
import tyqu.TableScope
import java.sql.ResultSet
import scala.quoted.ToExpr.StringToExpr

object RefinedResult:

  transparent inline def refinedResult[S <: Scope](rs: ResultSet): ResultIterator[?] = ${ refinedResultImpl[S]('rs) }

  private def refinedResultImpl[S <: Scope : Type](rs: Expr[ResultSet])(using Quotes): Expr[ResultIterator[?]] =
    import quotes.reflect.*

    val tableScope = Symbol.classSymbol("tyqu.TableScope")
    val tupleScope = Symbol.classSymbol("tyqu.TupleScope")
    val expression = Symbol.classSymbol("tyqu.Expression")

    val scopeType = TypeRepr.of[S]

    type AccessorMapExpr = Expr[Map[String, ResultSet => Any]]
    val emptyMapExpr = '{ Map[String, ResultSet => Any]() }

    def resultSetGetter(columnName: String, tpe: TypeRepr): Expr[(String, ResultSet => Any)] =
      val nameExpr = StringToExpr(columnName)
      val fn = tpe.asType match
        case '[String] =>
          '{ (rs: ResultSet) => rs.getString($nameExpr) }
        case '[Int] =>
          '{ (rs: ResultSet) => rs.getInt($nameExpr) }
        case '[Double] =>
          '{ (rs: ResultSet) => rs.getDouble($nameExpr) }
        case '[Boolean] =>
          '{ (rs: ResultSet) => rs.getBoolean($nameExpr) }
      '{ $nameExpr -> $fn }

    if scopeType.derivesFrom(expression) then
      val tpe = scopeType.baseType(expression).typeArgs(0)
      val getter = resultSetGetter("1", tpe)
      tpe.asType match
        case '[t] =>
          '{ ResultIterator($rs, rs => $getter._2(rs)).asInstanceOf[ResultIterator[t]] }
    
    else
      val (refinedType, accessorMapExpr): (TypeRepr, AccessorMapExpr) =
        if scopeType.derivesFrom(tableScope) then
          val classSymbol = scopeType.baseType(tableScope).typeArgs(0).classSymbol.get
          val fields = classSymbol.declaredFields
          fields.foldLeft((TypeRepr.of[Result], emptyMapExpr)) { (acc, field) =>
              val (refinementAcc, accessorAcc) = acc
              field.typeRef.translucentSuperType.asType match
                case '[Column[t]] =>
                  val getter = resultSetGetter(field.name, TypeRepr.of[t])
                  val refinementAcc2 = Refinement(refinementAcc, field.name, TypeRepr.of[t])
                  val accessorAcc2 = '{ $accessorAcc + $getter }
                  (refinementAcc2, accessorAcc2)
                case _ =>
                  (refinementAcc, accessorAcc)
          }
          
        else if scopeType.derivesFrom(tupleScope) then
          @tailrec
          def rec(tpe: TypeRepr, refinementAcc: TypeRepr, accessorAcc: AccessorMapExpr): (TypeRepr, AccessorMapExpr) = 
            tpe match
              case Refinement(parent, name, exprType) =>
                val valType = exprType.baseType(expression).typeArgs(0)
                val getter = resultSetGetter(name, valType)
                val refinementAcc2 = Refinement(refinementAcc, name, valType)
                val accessorAcc2 = '{ $accessorAcc + $getter }
                rec(parent, refinementAcc2, accessorAcc2)
              case _ => (refinementAcc, accessorAcc)
          rec(scopeType, TypeRepr.of[Result], '{ Map.empty })

        else
          throw new Exception("todo")
        
        end if

      refinedType.asType match
        case '[t] => '{
          def convertToResult(rs: ResultSet): t =
              val data = $accessorMapExpr.map{ (name, getter) =>
                (name -> getter(rs))  
              }
              Result(data).asInstanceOf[t]

          ResultIterator($rs, convertToResult)
        }

end RefinedResult
