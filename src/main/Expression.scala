package tyqu


type Numeric = Int | Long | Float | Double
type Primitive = Numeric | String | Char | Boolean


abstract sealed class Expression[T]:

  type NotNullColumns = EmptyTuple

  def as(n: String) = Alias[T, n.type](n, this)

  def asc = Asc(this)
  def desc = Desc(this)

  infix def ===(rhs: Expression[T]) = Equal(this, rhs)
  infix def =!=(rhs: Expression[T]) = NotEqual(this, rhs)

  def concat(rhs: Expression[?]) = Concat(this, rhs)

  def count = Count(this)

end Expression


abstract sealed class NamedExpression[T, N <: String & Singleton](val alias: N) extends Expression[T]

case class Alias[T, N <: String & Singleton](name: N, expression: Expression[T]) extends NamedExpression[T, N](name)

case class ColumnValue[T, N <: String & Singleton](name: N, relation: Relation) extends NamedExpression[T, N](name)

case class SubqueryExpression[T](qb: QueryBuilder[Expression[T]]) extends Expression[T]

case class LiteralExpression[T](value: T) extends Expression[T]

def lit[T](value: T) = LiteralExpression(value)

case class LessThan[T](lhs: Expression[T], rhs: Expression[T]) extends Expression[Boolean]
case class GreaterThan[T](lhs: Expression[T], rhs: Expression[T]) extends Expression[Boolean]
case class Equal[T](lhs: Expression[T], rhs: Expression[T]) extends Expression[Boolean]
case class NotEqual[T](lhs: Expression[T], rhs: Expression[T]) extends Expression[Boolean]

case class And(lhs: Expression[Boolean], rhs: Expression[Boolean]) extends Expression[Boolean]
case class Or(lhs: Expression[Boolean], rhs: Expression[Boolean]) extends Expression[Boolean]
case class Not(expression: Expression[Boolean]) extends Expression[Boolean]

case class CountAll() extends Expression[Int]
case class Count(expr: Expression[_]) extends Expression[Int]
case class Min(expr: Expression[_]) extends Expression[Int]
case class Max(expr: Expression[_]) extends Expression[Int]
case class Average(expr: Expression[_]) extends Expression[Int]
case class Sum(expr: Expression[_]) extends Expression[Int]

case class IsNull(expression: Expression[_]) extends Expression[Boolean]
case class IsNotNull(expression: Expression[_]) extends Expression[Boolean]
case class GetOrElse[T1, T2](mainEpression: Expression[T1 | Null], fallbackExpression: Expression[T2]) extends Expression[T1 | T2]

case class Plus[T](lhs: Expression[T], rhs: Expression[T]) extends Expression[T]
case class Minus[T](lhs: Expression[T], rhs: Expression[T]) extends Expression[T]
case class Multiply[T](lhs: Expression[T], rhs: Expression[T]) extends Expression[T]
case class Divide[T](lhs: Expression[T], rhs: Expression[T]) extends Expression[T]

case class Concat(lhs: Expression[?], rhs: Expression[?]) extends Expression[String]


extension [T, N <: String & Singleton](expression: NamedExpression[T | Null, N]) {
  def isNotNull = IsNotNull(expression).asInstanceOf[IsNotNull & { type NotNullColumns = Tuple1[expression.alias.type] }]
}


extension [T](expression: Expression[T | Null]) {
  def isNull = IsNull(expression)
  def isNotNull = IsNotNull(expression)
  def getOrElse[T2](fallback: Expression[T2]) = GetOrElse(expression, fallback)
}


extension (lhs: Expression[Boolean]) {
  infix def &&(rhs: Expression[Boolean]): And & { type NotNullColumns = Tuple.Concat[lhs.NotNullColumns, rhs.NotNullColumns]} =
    And(lhs, rhs).asInstanceOf[And & { type NotNullColumns = Tuple.Concat[lhs.NotNullColumns, rhs.NotNullColumns]}]
  infix def ||(rhs: Expression[Boolean]) = Or(lhs, rhs)
  infix def unary_! = Not(lhs)
}


extension [T <: Numeric](lhs: Expression[T]) {
  infix def <(rhs: Expression[T]) = LessThan(lhs, rhs)
  infix def >(rhs: Expression[T]) = GreaterThan(lhs, rhs)

  infix def +(rhs: Expression[T]) = Plus(lhs, rhs)
  infix def -(rhs: Expression[T]) = Minus(lhs, rhs)
  infix def *(rhs: Expression[T]) = Multiply(lhs, rhs)
  infix def /(rhs: Expression[T]) = Divide(lhs, rhs)
}


extension [T <: Numeric | Null](lhs: Expression[T]) {
  def min = Min(lhs)
  def max = Max(lhs)
  def avg = Average(lhs)
  def sum = Sum(lhs)
}


given Conversion[String, Expression[String]] = LiteralExpression(_)
given Conversion[Int, Expression[Int]] = LiteralExpression(_)
given [T]: Conversion[QueryBuilder[Expression[T]], Expression[T]] = SubqueryExpression(_)
