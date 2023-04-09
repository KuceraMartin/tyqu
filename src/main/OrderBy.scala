package tyqu


type OrderBy = Expression[?, true] | ExplicitDirection


enum ExplicitDirection:
  case Asc(by: Expression[?, true])
  case Desc(by: Expression[?, true])


extension (e: Expression[?, true])
  def asc = ExplicitDirection.Asc(e)
  def desc = ExplicitDirection.Desc(e)
