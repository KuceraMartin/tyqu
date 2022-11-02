package tyqu


type OrderBy = Expression[?] | ExplicitDirection


abstract sealed class ExplicitDirection
case class Asc(by: Expression[?]) extends ExplicitDirection
case class Desc(by: Expression[?]) extends ExplicitDirection

