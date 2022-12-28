// package tyqu


// class QueryBuilderTest extends UnitTest:

//   test("filter type mismatch") {
//     val code = """
//         case object MyTable extends Table:
//           val id = Column[Int]()
//           val firstName = Column[String]()
//           val lastName = Column[String]()
//           val age = Column[Int]()

//         from(MyTable).filter(_.id === "***str$$$")
//       """

//     val errors = compileErrors(code)

//     assertContains(errors,
//       "error:",
//       "Found:    (\"***str$$$\" : String)",
//       "Required: tyqu.Expression[Int]",
//     )
//   }
