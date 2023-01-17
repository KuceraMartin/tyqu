import tyqu.{*, given}

case object People extends Table:
  val id = Column[Int]()
  val firstName = Column[String]()
  val lastName = Column[String]()
  val age = Column[Int]()


object UnexpectedError:
  def main(args: Array[String]) =
    from(People).map{ a => 1 - (a.id + a.age.getOrElse(0)) * 2 }
