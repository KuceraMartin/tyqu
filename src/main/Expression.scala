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
  def underlyingName: String = table._name
  def getColumnName(property: String) = table._getColumnName(property)
  def colToExpr(col: Column[?]) = table._colToExpr(col)(this.asInstanceOf[TableRelation[table.type]])
  def pk: NamedExpression[?, ?] = colToExpr(table._pk)

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

  infix def ===(rhs: Expression[T]) = Function[Boolean]("=", List(this, rhs))
  infix def =!=(rhs: Expression[T]) = Function[Boolean]("!=", List(this, rhs))

  def concat(rhs: Expression[_]) =
    Function[String]("CONCAT", List(this, rhs).flatMap{
      case Function("CONCAT", exprs) => exprs
      case expr => List(expr)
    })

  def count = Function[Int]("COUNT", List(this))

end Expression


abstract sealed class NamedExpression[T, N <: String & Singleton](val alias: N) extends Expression[T]

case class Alias[T, N <: String & Singleton](name: N, expression: Expression[T]) extends NamedExpression[T, N](name)

case class ColumnValue[T, N <: String & Singleton](name: N, relation: Relation) extends NamedExpression[T, N](name)

case class SubqueryExpression[T](qb: QueryBuilder[Expression[T]]) extends Expression[T]

case class LiteralExpression[T](value: T) extends Expression[T]

case class Function[T](name: String, arguments: List[Expression[_]]) extends Expression[T]

def lit[T](value: T) = LiteralExpression(value)


case class And(lhs: Expression[Boolean], rhs: Expression[Boolean]) extends Expression[Boolean]
case class Or(lhs: Expression[Boolean], rhs: Expression[Boolean]) extends Expression[Boolean]
case class Not(expression: Expression[Boolean]) extends Expression[Boolean]

case class CountAll() extends Expression[Int]

case class Plus[T](lhs: Expression[T], rhs: Expression[T]) extends Expression[T]
case class Minus[T](lhs: Expression[T], rhs: Expression[T]) extends Expression[T]
case class Multiply[T](lhs: Expression[T], rhs: Expression[T]) extends Expression[T]
case class Divide[T](lhs: Expression[T], rhs: Expression[T]) extends Expression[T]


extension (lhs: Expression[Boolean]) {
  infix def &&(rhs: Expression[Boolean]) =
    if (lhs == NoFilterExpression) rhs
    else And(lhs, rhs)
  infix def ||(rhs: Expression[Boolean]) = Or(lhs, rhs)
  infix def unary_! = Not(lhs)
}

extension [T <: Numeric](lhs: Expression[T]) {
  infix def <(rhs: Expression[T]) = Function[Boolean]("<", List(lhs, rhs))
  infix def <=(rhs: Expression[T]) = Function[Boolean]("<=", List(lhs, rhs))
  infix def >(rhs: Expression[T]) = Function[Boolean](">", List(lhs, rhs))
  infix def >=(rhs: Expression[T]) = Function[Boolean](">=", List(lhs, rhs))
  infix def +(rhs: Expression[T]) = Plus(lhs, rhs)
  infix def -(rhs: Expression[T]) = Minus(lhs, rhs)
  infix def *(rhs: Expression[T]) = Multiply(lhs, rhs)
  infix def /(rhs: Expression[T]) = Divide(lhs, rhs)

  def min = Function[Int]("MIN", List(lhs))
  def max = Function[Int]("MAX", List(lhs))
  def avg = Function[Int]("AVG", List(lhs))
  def sum = Function[Int]("SUM", List(lhs))
}

extension (lhs: Expression[_]) {
  infix def +(rhs: Expression[_]) = lhs.concat(rhs)
}

given Conversion[String, Expression[String]] = LiteralExpression(_)
given Conversion[Int, Expression[Int]] = LiteralExpression(_)
given [T]: Conversion[QueryBuilder[Expression[T]], Expression[T]] = SubqueryExpression(_)


object NoFilterExpression extends LiteralExpression(true)
