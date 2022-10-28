package tyqu

type Numeric = Int | Long | Float | Double

type ExpressionSpecificType[E] = E match
  case Expression[t] => Expression[t]
  case _ => Nothing


abstract sealed class Expression[+T]:
  def as(n: String) = Alias[T, n.type](n, this)

abstract sealed class NamedExpression[+T, N <: String & Singleton](val alias: N) extends Expression[T]
case class Alias[+T, N <: String & Singleton](name: N, expression: Expression[T]) extends NamedExpression[T, N](name)
case class ColumnValue[+T, A <: String & Singleton](internalName: A, name: String, relation: String) extends NamedExpression[T, A](internalName)
case class LiteralExpression[T](value: T) extends Expression[T]

case class LessThan[T](
    lhs: Expression[T],
    rhs: ExpressionSpecificType[lhs.type]
  ) extends Expression[Boolean]

case class GreaterThan[T](
    lhs: Expression[T],
    rhs: ExpressionSpecificType[lhs.type]
  ) extends Expression[Boolean]

case class Equal[T](
    lhs: Expression[T],
    rhs: ExpressionSpecificType[lhs.type]
  ) extends Expression[Boolean]

case class NotEqual[T](
    lhs: Expression[T],
    rhs: ExpressionSpecificType[lhs.type]
  ) extends Expression[Boolean]

case class Plus[T](
    lhs: Expression[T],
    rhs: ExpressionSpecificType[lhs.type]
  ) extends Expression[T]

case class Concat(lhs: Expression[?], rhs: Expression[?]) extends Expression[String]


extension [T](lhs: Expression[T]) {
  infix def ===(rhs: ExpressionSpecificType[lhs.type]) = Equal(lhs, rhs)
  infix def =!=(rhs: ExpressionSpecificType[lhs.type]) = NotEqual(lhs, rhs)
  def concat(rhs: Expression[?]) = Concat(lhs, rhs)
}

extension [T <: Numeric](lhs: Expression[T]) {
  infix def <(rhs: ExpressionSpecificType[lhs.type]) = LessThan(lhs, rhs)
  infix def >(rhs: ExpressionSpecificType[lhs.type]) = GreaterThan(lhs, rhs)
  infix def +(rhs: ExpressionSpecificType[lhs.type]) = Plus(lhs, rhs)
}

given Conversion[Int, Expression[Int]] with
  def apply(v: Int) = LiteralExpression(v)

given Conversion[String, Expression[String]] with
  def apply(v: String) = LiteralExpression(v)
