package tyqu.translators

import tyqu.platforms.MySqlPlatform
import tyqu.{*, given}


class GenericSqlTranslatorSubqueriesTest extends UnitTest:

  case object Releases extends Table:
    val id = Column[Int]()
    val title = Column[String]()
    val country = Column[String]()
    val genre = Column[String]()

  case object Tracks extends Table:
    val id = Column[Int]()
    val position = Column[String]()
    val title = Column[String]()
    val duration = Column[Int]()
    val releaseId = Column[Int]()


  val translator = new GenericSqlTranslator(MySqlPlatform)


  test("map to subquery") {
    val query = translator.translate(
        from(Releases)
          .map{ r => (
              r.title,
              (
                from(Tracks)
                  .filter(_.releaseId === r.id)
                  .count
              ).as("tracks"),
            )
          }
      )

    assertEquals(query,
      """|SELECT `releases`.`title`, (
         |  SELECT COUNT(*)
         |  FROM `tracks`
         |  WHERE `tracks`.`release_id` = `releases`.`id`
         |) AS `tracks`
         |FROM `releases`""".stripMargin)
  }


  test("filter by subquery") {
    val query = translator.translate(
        from(Releases)
          .filter{ r =>
            from(Tracks).filter(_.releaseId === r.id)
              .count > 5
          }
          .map(_.title)
      )

    assertEquals(query,
      """|SELECT `releases`.`title`
         |FROM `releases`
         |WHERE (
         |  SELECT COUNT(*)
         |  FROM `tracks`
         |  WHERE `tracks`.`release_id` = `releases`.`id`
         |) > 5""".stripMargin)
  }


  test("sort by subquery") {
    val query = translator.translate(
        from(Releases)
          .sortBy{ r =>
            (from(Tracks).filter(_.releaseId === r.id).count).desc
          }
          .map(_.title)
      )

    assertEquals(query,
      """|SELECT `releases`.`title`
         |FROM `releases`
         |ORDER BY (
         |  SELECT COUNT(*)
         |  FROM `tracks`
         |  WHERE `tracks`.`release_id` = `releases`.`id`
         |) DESC""".stripMargin)
  }


