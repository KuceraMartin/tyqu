package tyqu.execution

import scala.annotation.tailrec
import scala.quoted.*

import tyqu.*
import tyqu.translators.RuntimeParameter
import tyqu.translators.SqlQuery
import tyqu.translators.Translator
import java.sql.ResultSet

class PreparedQuery[R](conversion: ResultSet => R, query: SqlQuery) extends Selectable:

  def applyDynamic(name: String)(args: Any*)(using executor: QueryExecutor): Any =
    assert(name == "execute")
    @tailrec
    def emplaceParameters(queryParams: List[Any], argParams: List[Any], acc: Vector[Any] = Vector.empty): Vector[Any] =
      (queryParams, argParams) match
        case ((p: RuntimeParameter) :: qs, a :: as) =>
          emplaceParameters(qs, as, acc :+ a)
        case (q :: qs, _) =>
          emplaceParameters(qs, argParams, acc :+ q)
        case (qs, Nil) =>
          acc ++ qs
        case _ =>
          throw Exception(s"$queryParams, $argParams")
    // println(emplaceParameters(query.parameters, args.toSeq))
    executor.execute(conversion, query.copy(parameters = emplaceParameters(query.parameters.toList, args.toList)))

end PreparedQuery


object PreparedQuery:

  trait Refinement[T]

  type ResultSubtype[T <: Result] = T

  def createCompileTimeExpr[S <: Scope : Type](qb: QueryBuilder[S], translator: Translator)(using Quotes): Expr[PreparedQuery[?]] =
    import quotes.reflect.*

    val translated = translator.translate(qb)
    val staticParams = translated.parameters.map{
      case x: Int => Expr(x)
      case r: RuntimeParameter => '{ RuntimeParameter[r.Type]("") } // todo https://dotty.epfl.ch/api/scala/reflect/ClassTag.html
      // case r: RuntimeParameter[Int] => '{RuntimeParameter[Int]("")}
      // case r: RuntimeParameter[String] => '{RuntimeParameter[String]("")}
    }
    val (runtimeParamNames, runtimeParamTypes) = translated.parameters.collect{
        case r: RuntimeParameter => (r.name, TypeRepr.of[r.Type])
      }
      .unzip
    val q = '{ SqlQuery(${Expr(translated.query)}, ${Expr.ofSeq(staticParams)}) }

    RefinedResult.refinedResultType[S] match
      case '[ResultSubtype[r]] =>
        type PreparedQuerySubtype[T <: PreparedQuery[r]] = T
        val executeMethod = MethodType(runtimeParamNames.toList)(_ => runtimeParamTypes.toList, _ => TypeRepr.of[Iterator[r]])
        val preparedQueryRefinement = Refinement(TypeRepr.of[PreparedQuery[r]], "execute", executeMethod)
        val conversion =
          (qb.scope match
            case s: MultiScope =>
              val cols = Expr.ofSeq(s.toList.map(c => Expr(c.alias)))
              '{ (rs: ResultSet) => Result($cols.map{ n => (n -> rs.getObject(n)) }.toMap).asInstanceOf[r] }
            case e: AnyExpression =>
              '{ (rs: ResultSet) => rs.getObject(1).asInstanceOf[r] }
          ).asExprOf[ResultSet => r]
        preparedQueryRefinement.asType match
          case '[PreparedQuerySubtype[p]] =>
            '{ PreparedQuery($conversion, $q).asInstanceOf[p] }

end PreparedQuery
