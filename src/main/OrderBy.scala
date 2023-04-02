package tyqu


type OrderBy = Expression[?, true] | ExplicitDirection


abstract sealed class ExplicitDirection
case class Asc(by: Expression[?, true]) extends ExplicitDirection
case class Desc(by: Expression[?, true]) extends ExplicitDirection

