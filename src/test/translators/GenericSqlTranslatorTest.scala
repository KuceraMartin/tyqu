package tyqu

import tyqu.platforms.MySqlPlatform
import tyqu.translators.GenericSqlTranslator


class GenericSqlTranslatorTest extends UnitTest:

  val table = Table(
      tableName = "t",
      columns = (
        column[Int]("id"),
        column[String]("firstName", "first_name"),
        column[String]("lastName", "last_name"),
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

	
  test("complex") {
    val query = translator.translate(
        from(table).map{ t => (t.id, t.firstName.concat(t.lastName).as("name")) }
                   .sortBy{ t => (t.name.asc, t.id.desc) }
      )

    assertEquals(query,
      """|SELECT `t`.`id`, CONCAT(`t`.`first_name`, `t`.`last_name`) AS `name`
         |FROM `t`
         |ORDER BY `name` ASC, `t`.`id` DESC""".stripMargin)
  }
