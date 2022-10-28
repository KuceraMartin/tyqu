package tyqu


class QueryBuilderTest extends UnitTest:

  test("filter type mismatch") {
    val code = """
        val table = Table(
          tableName = "t",
          columns = (
            column[Int]("id"),
            column[String]("name"),
          ),
        )
        from(table).filter(_.id === "***str$$$")
      """

    val errors = compileErrors(code)

    assert(List(
      "error:",
      "Found:    (\"***str$$$\" : String)",
      "Required: tyqu.ExpressionSpecificType[(?1 : tyqu.ColumnValue[Int, (\"id\" : String)])]"
    ).forall(errors.contains))
  }
