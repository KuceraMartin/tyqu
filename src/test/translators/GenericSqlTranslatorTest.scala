package tyqu

import tyqu.platforms.MySqlPlatform
import tyqu.translators.GenericSqlTranslator


class GenericSqlTranslatorTest extends UnitTest:

  val table = Table(
      tableName = "t",
      columns = (
        column[Int]("id"),
        column[String]("firstName"),
        column[String]("lastName"),
        column[Int]("age"),
      ),
    )
  
  val translator = new GenericSqlTranslator(MySqlPlatform)


  test("filter ===") {
    val query = translator.translate(
        from(table).filter(_.id === 1)
      )

    assertEquals(
      query,
      """|SELECT `t`.`id`, `t`.`first_name`, `t`.`last_name`, `t`.`age`
         |FROM `t`
         |WHERE `t`.`id` = 1""".stripMargin
    )
  }


  test("filter =!=") {
    val query = translator.translate(
        from(table).filter(_.firstName =!= "John")
      )

    assertEquals(
      query,
      """|SELECT `t`.`id`, `t`.`first_name`, `t`.`last_name`, `t`.`age`
         |FROM `t`
         |WHERE `t`.`first_name` != 'John'""".stripMargin,
    )
  }


  test("filter complex") {
    val query = translator.translate(
        from(table).filter{ t =>
          t.age > 18 && !(t.firstName === "John" || t.lastName === "Doe")
        }
      )

    assertEquals(
      query,
      """|SELECT `t`.`id`, `t`.`first_name`, `t`.`last_name`, `t`.`age`
         |FROM `t`
         |WHERE `t`.`age` > 18 AND NOT (`t`.`first_name` = 'John' OR `t`.`last_name` = 'Doe')""".stripMargin,
    )
  }


  test("map to Tuple3") {
    val query = translator.translate(
        from(table).map{ t => (t.id, t.firstName, t.lastName) }
      )

    assertEquals(
      query,
      """|SELECT `t`.`id`, `t`.`first_name`, `t`.`last_name`
         |FROM `t`""".stripMargin,
    )
  }


  test("map to Tuple2 + Tuple1") {
    val query = translator.translate(
        from(table).map{ t => (t.id, t.firstName) :* t.lastName }
      )
    
    assertEquals(
      query,
      """|SELECT `t`.`id`, `t`.`first_name`, `t`.`last_name`
         |FROM `t`""".stripMargin,
    )
  }


  test("map to Tuple1 + Tuple2") {
    val query = translator.translate(
        from(table).map{ t => t.id *: (t.firstName, t.lastName) }
      )

    assertEquals(
      query,
      """|SELECT `t`.`id`, `t`.`first_name`, `t`.`last_name`
         |FROM `t`""".stripMargin,
    )
  }


  test("map to 3 * Tuple1") {
    val query = translator.translate(
        from(table).map{ t => t.id *: t.firstName *: t.lastName *: EmptyTuple }
      )

    assertEquals(
      query,
      """|SELECT `t`.`id`, `t`.`first_name`, `t`.`last_name`
         |FROM `t`""".stripMargin,
    )
  }


  test("map to Tuple1") {
    val query = translator.translate(
        from(table).map(_.id)
      )

    assertEquals(
      query,
      """|SELECT `t`.`id`
         |FROM `t`""".stripMargin,
    )
  }


  test("map with alias") {
    val query = translator.translate(
        from(table).map{ t => (t.id, t.firstName.as("fn"), t.lastName.as("ln")) }
      )

    assertEquals(
      query,
      """|SELECT `t`.`id`, `t`.`first_name` AS `fn`, `t`.`last_name` AS `ln`
         |FROM `t`""".stripMargin,
    )
  }


  test("renamed columns accessible after map") {
    val query = translator.translate(
        from(table).map{ t => (t.id, t.firstName.as("fn"), t.lastName.as("ln")) }
                   .map(_.ln)
      )

    assertEquals(
      query,
      """|SELECT `t`.`last_name` AS `ln`
         |FROM `t`""".stripMargin,
    )
  }


  test("map complex") {
    val query = translator.translate(
        from(table).map{ t => (1 - (t.id + t.age) * 2).as("n") }
      )

    assertEquals(
      query,
      """|SELECT 1 - (`t`.`id` + `t`.`age`) * 2 AS `n`
         |FROM `t`""".stripMargin,
    )
  }


  test("order by many columns") {
    val query = translator.translate(
        from(table).sortBy{ t => (Desc(t.lastName), Asc(t.age), Asc(t.id)) }
      )

      assertEquals(query,
      """|SELECT `t`.`id`, `t`.`first_name`, `t`.`last_name`, `t`.`age`
         |FROM `t`
         |ORDER BY `t`.`last_name` DESC, `t`.`age` ASC, `t`.`id` ASC""".stripMargin)
  }


  test("order by one column") {
    val query = translator.translate(
        from(table).sortBy(_.id)
      )

      assertEquals(query,
      """|SELECT `t`.`id`, `t`.`first_name`, `t`.`last_name`, `t`.`age`
         |FROM `t`
         |ORDER BY `t`.`id`""".stripMargin)
  }


  test("sort by something that is not in scope".ignore) {
    val query = translator.translate(
        from(table).map{ t => (t.firstName, t.age.as("a")) }
                   .sortBy(_.a)
                   .map(_.firstName)
      )

    assertEquals(query,
      """|SELECT `t`.`first_name`, `t`.`age` AS a
         |FROM `t`
         |ORDER BY `a`""".stripMargin)
  }


  test("limit") {
    val query = translator.translate(
        from(table).limitBy(10)
      )

      assertEquals(query,
      """|SELECT `t`.`id`, `t`.`first_name`, `t`.`last_name`, `t`.`age`
         |FROM `t`
         |LIMIT 10""".stripMargin)
  }


  test("limit and offset") {
    val query = translator.translate(
        from(table).limitBy(15, 20)
      )

      assertEquals(query,
      """|SELECT `t`.`id`, `t`.`first_name`, `t`.`last_name`, `t`.`age`
         |FROM `t`
         |LIMIT 15
         |OFFSET 20""".stripMargin)
  }

	
  test("complex") {
    val query = translator.translate(
        from(table).filter{ _.age > 18 }
                   .map{ t => (t.id, t.firstName.concat(t.lastName).as("name")) }
                   .sortBy{ t => (t.name.asc, t.id.desc) }
                   .limitBy(5, 10)
      )

    assertEquals(query,
      """|SELECT `t`.`id`, CONCAT(`t`.`first_name`, `t`.`last_name`) AS `name`
         |FROM `t`
         |WHERE `t`.`age` > 18
         |ORDER BY `name` ASC, `t`.`id` DESC
         |LIMIT 5
         |OFFSET 10""".stripMargin)
  }
