package tyqu

import scala.quoted.*


type ScopeSubtype[T <: Scope] = T

object ScopeFactory:

  transparent inline def create[T <: Tuple](inline schema: T, inline from: String, inline where: Array[Expression[Boolean]]) = ${createImpl('schema, 'from, 'where)}

  private def createImpl[T <: Tuple](schema: Expr[T], from: Expr[String], where: Expr[Array[Expression[Boolean]]])(using q: Quotes, t: Type[T]) =
    import quotes.reflect.*

    def refine(t: TypeRepr, acc: TypeRepr): TypeRepr =
      t match
        case AppliedType(_, lst) => lst match
          case List(TypeRef(_, _), ConstantType(name)) =>
            Refinement(acc, name.value.asInstanceOf[String], t)
          case List(head, tail) => head match
            case AppliedType(_, List(_, ConstantType(name))) => 
              refine(tail, Refinement(acc, name.value.asInstanceOf[String], head))
          case l: List[AppliedType] =>
            l.foldLeft(acc) { (acc2, t2) => refine(t2, acc2) }
        case TypeRef(_, _) | TermRef(_, _) => acc

    val refinementType = refine(schema.asTerm.tpe.dealias.widen, TypeRepr.of[Scope])

    refinementType.asType match
      case '[ScopeSubtype[t]] => '{new QueryBuilder(Scope($schema), $from, $where).asInstanceOf[QueryBuilder[t]]}
