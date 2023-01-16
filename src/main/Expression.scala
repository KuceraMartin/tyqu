package tyqu


type Numeric = Int | Long | Float | Double
type Primitive = Numeric | String | Char | Boolean


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
  def pk: NamedExpression[?, ?] = colToExpr(table.pk)

enum JoinType:
  case Inner, Left, Right, FullOuter

case class FromRelation[T <: Table](t: T) extends TableRelation(t)

case class JoinRelation[T <: Table](t: T, joinType: JoinType, on: JoinRelation[T] => Expression[Boolean]) extends TableRelation(t)

case class SubqueryRelation(qb: QueryBuilder[?]) extends Relation:
  def underlyingName: String = qb.from.underlyingName
  def getColumnName(property: String) = qb.from.getColumnName(property)


abstract sealed class Expression[T]:

  def as(n: String) = Alias[T, n.type](n, this)

  def asc = Asc(this)
  def desc = Desc(this)

  infix def ===[T2 <: T | Null](rhs: Expression[T2]) = Function[Boolean]("=", List(this, rhs))
  infix def =!=[T2 <: T | Null](rhs: Expression[T2]) = Function[Boolean]("!=", List(this, rhs))

  def concat(rhs: Expression[?]) =
    Function[String]("CONCAT", List(this, rhs).flatMap{
      case Function("CONCAT", exprs) => exprs
      case expr => List(expr)
    })

  def count = Function[Int]("COUNT", List(this))

end Expression


abstract sealed class NamedExpression[T, N <: String & Singleton](val alias: N) extends Expression[T]

case class Alias[T, N <: String & Singleton](name: N, expression: Expression[T]) extends NamedExpression[T, N](name)

case class ColumnValue[T, N <: String & Singleton](name: N, relation: Relation) extends NamedExpression[T, N](name)

// E because the type in QueryBuilder is invariant
case class SubqueryExpression[T, E <: Expression[T]](qb: QueryBuilder[E]) extends Expression[T]

case class LiteralExpression[T](value: T) extends Expression[T]

case class Function[T](name: String, arguments: List[Expression[?]]) extends Expression[T]

def lit[T](value: T) = LiteralExpression(value)


case class And(lhs: Expression[Boolean], rhs: Expression[Boolean]) extends Expression[Boolean]
case class Or(lhs: Expression[Boolean], rhs: Expression[Boolean]) extends Expression[Boolean]
case class Not(expression: Expression[Boolean]) extends Expression[Boolean]
case class IsNull[T](expression: Expression[T | Null]) extends Expression[Boolean]
case class IsNotNull[T](expression: Expression[T | Null]) extends Expression[Boolean]
case class Exists(subquery: SubqueryExpression[?, ?]) extends Expression[Boolean]
case class StartsWith(needle: String, haystack: Expression[String]) extends Expression[Boolean]
case class EndsWith(needle: String, haystack: Expression[String]) extends Expression[Boolean]
case class Contains(needle: String, haystack: Expression[String]) extends Expression[Boolean]

case class CountAll() extends Expression[Int]

case class Plus[T1 <: Numeric | Null, T2 <: Numeric | Null](lhs: Expression[T1], rhs: Expression[T2]) extends Expression[T1 | T2]
case class Minus[T1 <: Numeric | Null, T2 <: Numeric | Null](lhs: Expression[T1], rhs: Expression[T2]) extends Expression[T1 | T2]
case class Multiply[T1 <: Numeric | Null, T2 <: Numeric | Null](lhs: Expression[T1], rhs: Expression[T2]) extends Expression[T1 | T2]
case class Divide[T1 <: Numeric | Null, T2 <: Numeric | Null](lhs: Expression[T1], rhs: Expression[T2]) extends Expression[T1 | T2]


extension (lhs: Expression[Boolean])
  infix def &&(rhs: Expression[Boolean]) =
    if (lhs == NoFilterExpression) rhs
    else And(lhs, rhs)
  infix def ||(rhs: Expression[Boolean]) = Or(lhs, rhs)
  infix def unary_! = Not(lhs)


extension [T <: Numeric | Null] (lhs: Expression[T])
  infix def <[T2 <: Numeric | Null](rhs: Expression[T2]) = Function[Boolean]("<", List(lhs, rhs))
  infix def <=[T2 <: Numeric | Null](rhs: Expression[T2]) = Function[Boolean]("<=", List(lhs, rhs))
  infix def >[T2 <: Numeric | Null](rhs: Expression[T2]) = Function[Boolean](">", List(lhs, rhs))
  infix def >=[T2 <: Numeric | Null](rhs: Expression[T2]) = Function[Boolean](">=", List(lhs, rhs))
  infix def +[T2 <: Numeric | Null](rhs: Expression[T2]) = Plus(lhs, rhs)
  infix def -[T2 <: Numeric | Null](rhs: Expression[T2]) = Minus(lhs, rhs)
  infix def *[T2 <: Numeric | Null](rhs: Expression[T2]) = Multiply(lhs, rhs)
  infix def /[T2 <: Numeric | Null](rhs: Expression[T2]) = Divide(lhs, rhs)


extension [T] (lhs: Expression[T | Null])
  def isNull = IsNull(lhs)
  def isNotNull = IsNotNull(lhs)
  def getOrElse[T2](fallback: Expression[T2]) = Function[T | T2]("COALESCE", List(lhs, fallback))


extension (lhs: Expression[?])
  infix def +(rhs: Expression[?]) = lhs.concat(rhs)


extension (lhs: Expression[String])
  def startsWith(rhs: String) = StartsWith(needle = rhs, haystack = lhs)
  def endsWith(rhs: String) = EndsWith(needle = rhs, haystack = lhs)
  def contains(rhs: String) = Contains(needle = rhs, haystack = lhs)


given Conversion[String, Expression[String]] = LiteralExpression(_)
given Conversion[Int, Expression[Int]] = LiteralExpression(_)
given [T, E <: Expression[T]]: Conversion[QueryBuilder[E], Expression[T]] = SubqueryExpression(_)


object NoFilterExpression extends LiteralExpression(true)
