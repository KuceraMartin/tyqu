package tyqu.translators

import tyqu.platforms.MySqlPlatform
import tyqu.{*, given}


class GenericSqlTranslatorTest extends UnitTest:

  case object MyTable extends Table:
    val id = Column[Int]()
    val firstName = Column[String]()
    val lastName = Column[String]()
    val age = Column[Int]()

  val translator = new GenericSqlTranslator(MySqlPlatform)


  test("filter ===") {
    val query = translator.translate(
        from(MyTable).filter(_.id === 1)
      )

    assertEquals(
      query,
      """|SELECT `my_table`.*
         |FROM `my_table`
         |WHERE `my_table`.`id` = 1""".stripMargin
    )
  }


  test("filter =!=") {
    val query = translator.translate(
        from(MyTable).filter(_.firstName =!= "John")
      )

    assertEquals(
      query,
      """|SELECT `my_table`.*
         |FROM `my_table`
         |WHERE `my_table`.`first_name` != 'John'""".stripMargin,
    )
  }


  test("filter complex") {
    val query = translator.translate(
        from(MyTable).filter{ t =>
          t.age > 18 && !(t.firstName === "John" || t.lastName === "Doe")
        }
      )

    assertEquals(
      query,
      """|SELECT `my_table`.*
         |FROM `my_table`
         |WHERE `my_table`.`age` > 18 AND NOT (`my_table`.`first_name` = 'John' OR `my_table`.`last_name` = 'Doe')""".stripMargin,
    )
  }


  test("map to Tuple3") {
    val query = translator.translate(
        from(MyTable).map{ t => (t.id, t.firstName, t.lastName) }
      )

    assertEquals(
      query,
      """|SELECT `my_table`.`id`, `my_table`.`first_name`, `my_table`.`last_name`
         |FROM `my_table`""".stripMargin,
    )
  }


  test("map to Tuple2 + Tuple1") {
    val query = translator.translate(
        from(MyTable).map{ t => (t.id, t.firstName) :* t.lastName }
      )
    
    assertEquals(
      query,
      """|SELECT `my_table`.`id`, `my_table`.`first_name`, `my_table`.`last_name`
         |FROM `my_table`""".stripMargin,
    )
  }


  test("map to Tuple1 + Tuple2") {
    val query = translator.translate(
        from(MyTable).map{ t => t.id *: (t.firstName, t.lastName) }
      )

    assertEquals(
      query,
      """|SELECT `my_table`.`id`, `my_table`.`first_name`, `my_table`.`last_name`
         |FROM `my_table`""".stripMargin,
    )
  }


  test("map to 3 * Tuple1") {
    val query = translator.translate(
        from(MyTable).map{ t => t.id *: t.firstName *: t.lastName *: EmptyTuple }
      )

    assertEquals(
      query,
      """|SELECT `my_table`.`id`, `my_table`.`first_name`, `my_table`.`last_name`
         |FROM `my_table`""".stripMargin,
    )
  }


  test("map to Tuple1") {
    val query = translator.translate(
        from(MyTable).map(_.age + 2)
      )

    assertEquals(
      query,
      """|SELECT `my_table`.`age` + 2
         |FROM `my_table`""".stripMargin,
    )
  }


  test("map with alias") {
    val query = translator.translate(
        from(MyTable).map{ t => (t.id, t.firstName.as("fn"), t.lastName.as("ln")) }
      )

    assertEquals(
      query,
      """|SELECT `my_table`.`id`, `my_table`.`first_name` AS `fn`, `my_table`.`last_name` AS `ln`
         |FROM `my_table`""".stripMargin,
    )
  }


  test("renamed columns accessible after map") {
    val query = translator.translate(
        from(MyTable).map{ t => (t.id, t.firstName.as("fn"), t.lastName.as("ln")) }
                   .map(_.ln)
      )

    assertEquals(
      query,
      """|SELECT `my_table`.`last_name` AS `ln`
         |FROM `my_table`""".stripMargin,
    )
  }


  test("map complex") {
    val query = translator.translate(
        from(MyTable).map{ t => (1 - (t.id + t.age) * 2).as("n") }
      )

    assertEquals(
      query,
      """|SELECT 1 - (`my_table`.`id` + `my_table`.`age`) * 2 AS `n`
         |FROM `my_table`""".stripMargin,
    )
  }


  test("order by many columns") {
    val query = translator.translate(
        from(MyTable).sortBy{ t => (Desc(t.lastName), Asc(t.age), Asc(t.id)) }
      )

      assertEquals(query,
      """|SELECT `my_table`.*
         |FROM `my_table`
         |ORDER BY `my_table`.`last_name` DESC, `my_table`.`age` ASC, `my_table`.`id` ASC""".stripMargin)
  }


  test("order by one column") {
    val query = translator.translate(
        from(MyTable).sortBy(_.id)
      )

      assertEquals(query,
      """|SELECT `my_table`.*
         |FROM `my_table`
         |ORDER BY `my_table`.`id`""".stripMargin)
  }


  test("sort by something that is not in scope".ignore) {
    val query = translator.translate(
        from(MyTable).map{ t => (t.firstName, t.age.as("a")) }
                   .sortBy(_.a)
                   .map(_.firstName)
      )

    assertEquals(query,
      """|SELECT `my_table`.`first_name`, `my_table`.`age` AS a
         |FROM `my_table`
         |ORDER BY `a`""".stripMargin)
  }


  test("limit") {
    val query = translator.translate(
        from(MyTable).limitBy(10)
      )

      assertEquals(query,
      """|SELECT `my_table`.*
         |FROM `my_table`
         |LIMIT 10""".stripMargin)
  }


  test("limit and offset") {
    val query = translator.translate(
        from(MyTable).limitBy(15, 20)
      )

      assertEquals(query,
      """|SELECT `my_table`.*
         |FROM `my_table`
         |LIMIT 15
         |OFFSET 20""".stripMargin)
  }

	
  test("complex") {
    val query = translator.translate(
        from(MyTable).filter{ _.age > 18 }
                   .map{ t => (t.id, t.firstName.concat(t.lastName).as("name")) }
                   .sortBy{ t => (t.name.asc, t.id.desc) }
                   .limitBy(5, 10)
      )

    assertEquals(query,
      """|SELECT `my_table`.`id`, CONCAT(`my_table`.`first_name`, `my_table`.`last_name`) AS `name`
         |FROM `my_table`
         |WHERE `my_table`.`age` > 18
         |ORDER BY `name` ASC, `my_table`.`id` DESC
         |LIMIT 5
         |OFFSET 10""".stripMargin)
  }
