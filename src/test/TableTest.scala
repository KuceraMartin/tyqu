package tyqu:

  class TableTest extends UnitTest:

    test("columns need to be instances of Column") {
      val code = """
          object MyTable extends Table("t"):
            val id = column[Int]("id")
            val firstName = column[String]("first_name")
            val someProp = other.Column("other column")

          from(MyTable)
        """

      val errors = compileErrors(code)

      assertContains(errors,
        "Exception occurred while executing macro expansion.",
        "tyqu.TableDefinitionException: Table MyTable has property someProp of type other.Column which is not an allowed member of a table definition!",
      )
    }

end tyqu


package other:

  case class Column[T](name: String)

end other
