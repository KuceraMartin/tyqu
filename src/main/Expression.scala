package tyqu

import scala.annotation.targetName

import utils.IsTupleOf
import Tuple.Fold
import scala.compiletime.ops.boolean.*


type Numeric = Int | Long | Float | Double
type Primitive = Numeric | String | Char | Boolean

type ForAll[T <: Tuple, Pred[X] <: Boolean] <: Boolean = T match
  case EmptyTuple => true
  case h *: t => Pred[h] && ForAll[t, Pred]

type CanSelectExpr[T] <: Boolean = T match
  case Expression[?, true] => true
  case Expression[?, false] => false
  case _ => Nothing

type ArgsCanSelect[T] <: Boolean = T match
  case Tuple => ForAll[T, CanSelectExpr]
  case _ => CanSelectExpr[T]


sealed abstract class Relation:
  def underlyingName: String
  def getColumnName(property: String): String
  override def equals(x: Any): Boolean =
    x match
      case r: Relation => eq(r)
      case _ => false

abstract class TableRelation[T <: Table](val table: T) extends Relation:
  def underlyingName: String = table.tableName
  def getColumnName(property: String) = table.getColumnName(property)
  def colToExpr(col: Column[?]) = table.colToExpr(col)(this.asInstanceOf[TableRelation[table.type]])
  def pk: NamedExpression[?, ?, ?] = colToExpr(table.pk)

enum JoinType:
  case Inner, Left, Right, FullOuter

case class FromRelation[T <: Table](t: T) extends TableRelation(t)

case class JoinRelation[T <: Table](t: T, joinType: JoinType, on: JoinRelation[T] => Expression[Boolean, ?]) extends TableRelation(t)

case class SubqueryRelation(qb: QueryBuilder[?]) extends Relation:
  def underlyingName: String = qb.from.underlyingName
  def getColumnName(property: String) = qb.from.getColumnName(property)


abstract sealed class Expression[T, CanSelect <: Boolean]:

  def as(n: String) = Alias[T, CanSelect, n.type](n, this)

  infix def ===[T2 <: T | Null, E <: Expression[T2, ?]](rhs: E) = Function[Boolean]("=", (this, rhs))
  infix def =!=[T2 <: T | Null, E <: Expression[T2, ?]](rhs: E) = Function[Boolean]("!=", (this, rhs))

  def concat[E <: Expression[?, ?]](rhs: E) =
    Concat(this, rhs)

  def count = Aggregation[Int]("COUNT", this)

end Expression

type AnyExpression = Expression[?, ?]

abstract sealed class NamedExpression[T, CanSelect <: Boolean, N <: String & Singleton](val alias: N) extends Expression[T, CanSelect]:
  def underlyingName: String = alias

case class Alias[T, CanSelect <: Boolean, N <: String & Singleton](name: N, expression: Expression[T, CanSelect]) extends NamedExpression[T, CanSelect, N](name)

case class ColumnValue[T, CanSelect <: Boolean, N <: String & Singleton](name: N, relation: Relation) extends NamedExpression[T, CanSelect, N](name):
  override def underlyingName: String = relation.getColumnName(name)

// E because the type in QueryBuilder is invariant
case class SubqueryExpression[T, E <: Expression[T, true]](qb: QueryBuilder[E]) extends Expression[T, true]

case class LiteralValue[T](value: T, static: Boolean = false) extends Expression[T, true]

abstract class ProductExpression[T, Arguments <: Tuple | Expression[?, ?]] extends Expression[T, ArgsCanSelect[Arguments]]

case class Function[T, Arguments <: Tuple](name: String, arguments: Arguments)(using IsTupleOf[Arguments, Expression[?, ?]] =:= true) extends ProductExpression[T, Arguments]

object Function:
  final class ApplyHelper[T]:
    def apply[Arguments <: Tuple](name: String, arguments: Arguments)(using IsTupleOf[Arguments, Expression[?, ?]] =:= true) = new Function[T, Arguments](name, arguments)

    def apply[E <: Expression[?, ?]](name: String, argument: E) = new Function[T, Tuple1[E]](name, Tuple1(argument))

  def apply[T] = ApplyHelper[T]()

case class Aggregation[T, Arguments <: Tuple](name: String, arguments: Arguments)(using IsTupleOf[Arguments, Expression[?, ?]] =:= true) extends Expression[T | Null, true]

object Aggregation:
  final class ApplyHelper[T]:
    def apply[Arguments <: Tuple](name: String, arguments: Arguments)(using IsTupleOf[Arguments, Expression[?, ?]] =:= true) = new Aggregation[T, Arguments](name, arguments)

    def apply[E <: Expression[?, ?]](name: String, argument: E) = new Aggregation[T, Tuple1[E]](name, Tuple1(argument))

  def apply[T] = ApplyHelper[T]()
end Aggregation

abstract class UnaryNumericAggregation(name: String):
  def apply[T <: Numeric | Null](e: Expression[T, ?]) = Aggregation[T | Null](name, Tuple(e))
  def unapply(a: Aggregation[?, ?]): Option[AnyExpression] =
    if a.name == name then Some(a)
    else None
end UnaryNumericAggregation

object Sum extends UnaryNumericAggregation("SUM")
object Avg extends UnaryNumericAggregation("AVG")
object Min extends UnaryNumericAggregation("MIN")
object Max extends UnaryNumericAggregation("MAX")

def lit[T](value: T) = LiteralValue(value)

abstract class BinaryFunction[T](name: String):
  def apply[E1 <: Expression[?, ?], E2 <: Expression[?, ?]](a: E1, b: E2) =
    Function[T](name, (a, b))

  def unapply(f: Function[?, ?]): Option[(AnyExpression, AnyExpression)] =
    if f.name == name then Some(f.arguments.asInstanceOf[(AnyExpression, AnyExpression)])
    else None

object Concat extends BinaryFunction[String]("CONCAT")

case class And[L <: Expression[Boolean, ?], R <: Expression[Boolean, ?]](lhs: L, rhs: R) extends ProductExpression[Boolean, (L, R)]
case class Or[L <: Expression[Boolean, ?], R <: Expression[Boolean, ?]](lhs: L, rhs: R) extends ProductExpression[Boolean, (L, R)]
case class Not[E <: Expression[Boolean, ?]](expression: E) extends ProductExpression[Boolean, Tuple1[E]]
case class IsNull[T, S <: Boolean, E <: Expression[T | Null, S]](expression: E) extends ProductExpression[Boolean, Tuple1[E]]
case class IsNotNull[T, E <: Expression[T | Null, ?]](expression: E) extends ProductExpression[Boolean, E]
case class Exists(subquery: SubqueryExpression[?, ?]) extends Expression[Boolean, true]
case class StartsWith[S <: Boolean](needle: String, haystack: Expression[String, S]) extends Expression[Boolean, S]
case class EndsWith[S <: Boolean](needle: String, haystack: Expression[String, S]) extends Expression[Boolean, S]
case class Contains[S <: Boolean](needle: String, haystack: Expression[String, S]) extends Expression[Boolean, S]

case class CountAll() extends Expression[Int, true]

case class Plus[T1 <: Numeric | Null, T2 <: Numeric | Null, E1 <: Expression[T1, ?], E2 <: Expression[T2, ?]](lhs: E1, rhs: E2) extends ProductExpression[T1 | T2, (E1, E2)]
case class Minus[T1 <: Numeric | Null, T2 <: Numeric | Null, E1 <: Expression[T1, ?], E2 <: Expression[T2, ?]](lhs: E1, rhs: E2) extends ProductExpression[T1 | T2, (E1, E2)]
case class Multiply[T1 <: Numeric | Null, T2 <: Numeric | Null, E1 <: Expression[T1, ?], E2 <: Expression[T2, ?]](lhs: E1, rhs: E2) extends ProductExpression[T1 | T2, (E1, E2)]
case class Divide[T1 <: Numeric | Null, T2 <: Numeric | Null, E1 <: Expression[T1, ?], E2 <: Expression[T2, ?]](lhs: E1, rhs: E2) extends ProductExpression[T1 | T2, (E1, E2)]


extension [S <: Boolean] (lhs: Expression[Boolean, S])
  infix def &&[S2 <: Boolean](rhs: Expression[Boolean, S2]) =
    if (lhs == NoFilterExpression) rhs
    else And(lhs, rhs)
  infix def ||[S2 <: Boolean](rhs: Expression[Boolean, S2]) = Or(lhs, rhs)
  infix def unary_! = Not(lhs)


extension [T <: Numeric | Null, S <: Boolean] (lhs: Expression[T, S])
  infix def <[T2 <: Numeric | Null, S2 <: Boolean](rhs: Expression[T2, S2]) = Function[Boolean]("<", (lhs, rhs))
  infix def <=[T2 <: Numeric | Null, S2 <: Boolean](rhs: Expression[T2, S2]) = Function[Boolean]("<=", (lhs, rhs))
  infix def >[T2 <: Numeric | Null, S2 <: Boolean](rhs: Expression[T2, S2]) = Function[Boolean, (Expression[T, S], Expression[T2, S2])](">", (lhs, rhs))
  infix def >=[T2 <: Numeric | Null, S2 <: Boolean](rhs: Expression[T2, S2]) = Function[Boolean](">=", (lhs, rhs))
  infix def +[T2 <: Numeric | Null, S2 <: Boolean](rhs: Expression[T2, S2]) = Plus(lhs, rhs)
  infix def -[T2 <: Numeric | Null, S2 <: Boolean](rhs: Expression[T2, S2]) = Minus(lhs, rhs)
  infix def *[T2 <: Numeric | Null, S2 <: Boolean](rhs: Expression[T2, S2]) = Multiply(lhs, rhs)
  infix def /[T2 <: Numeric | Null, S2 <: Boolean](rhs: Expression[T2, S2]) = Divide(lhs, rhs)


extension [T, S <: Boolean] (lhs: Expression[T | Null, S])
  def isNull = IsNull(lhs)
  def isNotNull = IsNotNull(lhs)
  def getOrElse[T2, S2 <: Boolean](fallback: Expression[T2, S2]) = Function[T | T2]("COALESCE", (lhs, fallback))


extension (lhs: Expression[?, ?])
  infix def +[T, S <: Boolean](rhs: Expression[T, S]) = lhs.concat(rhs)


extension (lhs: Expression[String, ?])
  def startsWith(rhs: String) = StartsWith(needle = rhs, haystack = lhs)
  def endsWith(rhs: String) = EndsWith(needle = rhs, haystack = lhs)
  def contains(rhs: String) = Contains(needle = rhs, haystack = lhs)


extension [T <: Numeric | Null, E <: Expression[T, true]] (qb: QueryBuilder[E])
  def sum = qb.copy(scope = Sum(qb.scope))
  def min = qb.copy(scope = Min(qb.scope))
  def max = qb.copy(scope = Max(qb.scope))
  def avg = qb.copy(scope = Avg(qb.scope))


extension [T <: Numeric | Null] (e: Expression[T, false])
  def sum = Sum(e)
  def min = Min(e)
  def max = Max(e)
  def avg = Avg(e)


given Conversion[String, Expression[String, true]] = LiteralValue(_)
given Conversion[Int, Expression[Int, true]] = LiteralValue(_)
given [T, E <: Expression[T, true]]: Conversion[QueryBuilder[E], Expression[T, true]] = SubqueryExpression(_)


object NoFilterExpression extends LiteralValue(true)
