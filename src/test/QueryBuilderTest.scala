package tyqu


class QueryBuilderTest extends UnitTest:

  test("filter type mismatch") {
    val code = """
        case object MyTable extends Table:
          val id = Column[Int]()
          val firstName = Column[String]()
          val lastName = Column[String]()
          val age = Column[Int]()

        from(MyTable).filter(_.id === "***str$$$")
      """

    val errors = compileErrors(code)

    assertContains(errors,
      "error:",
      "Found:    (\"***str$$$\" : String)",
      "Required: tyqu.Expression[T2]",
      "where:    T2 is a type variable with constraint <: Int | Null",
    )
  }


  test("isNull method not available on non-nullable expressions") {
    val code = """
        case object MyTable extends Table:
          val id = Column[Int]()
          val firstName = Column[String]()
          val lastName = Column[String]()
          val age = Column[Int]()

        from(MyTable).filter(_.firstName.isNull)
      """

    val errors = compileErrors(code)

    assertContains(errors,
      "error:",
      "value isNull is not a member of",
    )
  }

  // test("getOrElse method not available on non-nullable expressions") {
  //   val code = """
  //       case object MyTable extends Table:
  //         val id = Column[Int]()
  //         val firstName = Column[String]()
  //         val lastName = Column[String]()
  //         val age = Column[Int]()

  //       from(MyTable).map{ t => 1 - (t.id + t.age.getOrElse(0)) * t.id }
  //     """

  //   val errors = compileErrors(code)

  //   assertContains(errors,
  //     "error:",
  //     "value getOrElse is not a member of",
  //   )
  // }
