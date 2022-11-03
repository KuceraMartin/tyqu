package tyqu


class TableTest extends UnitTest:

  test("columns need to be instances of Column") {
    val code = """
        val table = Table(
          tableName = "t",
          columns = (
            column[Int]("id"),
            column[String]("name"),
            "not a column"
          ),
        )
      """

    val errors = compileErrors(code)

    assert(
			errors.contains("error: t is not a tuple of S!")
		)
  }
