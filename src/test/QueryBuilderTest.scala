package tyqu


class QueryBuilderTest extends UnitTest:

  test("filter type mismatch") {
    val code = """
        object MyTable extends Table:
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
      "Required: tyqu.Expression[T2, ?]",
      "where:    T2 is a type variable with constraint <: Int | Null",
    )
  }


  test("isNull method not available on non-nullable expressions") {
    val code = """
        object MyTable extends Table:
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

  test("getOrElse method not available on m:1 non-nullable relation columns") {
    val code = """
      object People extends Table:
        val id = Column[Int]()
        val name = Column[String]()
        val roleId = Column[Int]()
        lazy val role = ManyToOne(Roles, roleId)

      object Roles extends Table:
        val id = Column[Int]()
        val title = Column[String]()

      from(People).map(_.role.title.getOrElse("none"))
    """

    val errors = compileErrors(code)

    assertContains(errors,
      "error:",
      "value getOrElse is not a member of",
    )
  }


  test("groupMap (group by expression, select expression)") {
    val code = """
      object MyTable extends Table:
        val id = Column[Int]()
        val firstName = Column[String]()
        val lastName = Column[String]()
        val age = Column[Int]()

      from(MyTable)
        .groupMap(_.firstName)(_.lastName)
    """

    val errors = compileErrors(code)

    assertContains(errors,
      "error:",
      "Cannot prove that tyqu.QueryBuilder.IsValidMapResult",
    )
  }

  test("groupMap (group by expression, select tuple)") {
    val code = """
      object MyTable extends Table:
        val id = Column[Int]()
        val firstName = Column[String]()
        val lastName = Column[String]()
        val age = Column[Int]()

      from(MyTable)
        .groupMap(_.firstName){ t => (t.firstName, t.lastName) }
    """

    val errors = compileErrors(code)

    assertContains(errors,
      "error:",
      "Cannot prove that tyqu.QueryBuilder.IsValidMapResult",
    )
  }

  test("groupMap (group by tuple, select expression)") {
    val code = """
      object MyTable extends Table:
        val id = Column[Int]()
        val firstName = Column[String]()
        val lastName = Column[String]()
        val age = Column[Int]()

      from(MyTable)
        .groupMap{ t => (t.firstName, t.lastName)}(_.age)
    """

    val errors = compileErrors(code)

    assertContains(errors,
      "error:",
      "Cannot prove that tyqu.QueryBuilder.IsValidMapResult",
    )
  }

  test("groupMap (group by tuple, select tuple)") {
    val code = """
      object MyTable extends Table:
        val id = Column[Int]()
        val firstName = Column[String]()
        val lastName = Column[String]()
        val age = Column[Int]()

      from(MyTable)
        .groupMap{ t => (t.firstName, t.lastName) }{ t => (t.firstName, t.lastName, t.age) }
    """

    val errors = compileErrors(code)

    assertContains(errors,
      "error:",
      "Cannot prove that tyqu.QueryBuilder.IsValidMapResult",
    )
  }
