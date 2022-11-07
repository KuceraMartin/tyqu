package tyqu


class QueryBuilderTest extends UnitTest:

  test("filter type mismatch") {
    val code = """
        object MyTable extends Table("t"):
          val id = column[Int]("id")
          val firstName = column[String]("first_name")
          val lastName = column[String]("last_name")
          val age = column[Int]("age")

        from(MyTable).filter(_.id === "***str$$$")
      """

    val errors = compileErrors(code)

    assertContains(errors,
      "error:",
      "Found:    (\"***str$$$\" : String)",
      "Required: tyqu.Expression[Int]",
    )
  }
