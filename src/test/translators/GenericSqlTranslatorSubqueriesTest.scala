package tyqu.translators

import scala.language.implicitConversions

import tyqu.platforms.MySqlPlatform
import tyqu.{*, given}


class GenericSqlTranslatorSubqueriesTest extends UnitTest:

  case object Releases extends Table:
    val id = Column[Int](primary = true)
    val title = Column[String]()
    val country = Column[String]()
    val genre = Column[String]()
    lazy val artists = ManyToMany(Artists, ReleasedBy, ReleasedBy.releaseId, ReleasedBy.artistId)
    lazy val tracks = OneToMany(Tracks, Tracks.release)

  case object Tracks extends Table:
    val id = Column[Int](primary = true)
    val position = Column[String]()
    val title = Column[String]()
    val duration = Column[Int]()
    val releaseId = Column[Int]()
    lazy val release = ManyToOne(Releases, releaseId)
  
  case object Artists extends Table:
    val id = Column[Int](primary = true)
    val name = Column[String]()
    val releases = ManyToMany(Releases, ReleasedBy, ReleasedBy.artistId, ReleasedBy.releaseId)
  
  case object ReleasedBy extends Table:
    val releaseId = Column[Int]()
    val artistId = Column[Int]()

  val translator = new GenericSqlTranslator(MySqlPlatform)


  test("map m:1") {
    val query = translator.translate(
      from(Tracks)
        .map{ t => (t.title, t.release.genre) }
    )

    assertEquals(query,
      """|SELECT `tracks`.`title`, `releases`.`genre`
         |FROM `tracks`
         |JOIN `releases` ON `tracks`.`release_id` = `releases`.`id`""".stripMargin)
  }


  test("map 1:m") {
    val query = translator.translate(
        from(Releases)
          .map{ r => (
              r.title,
              r.tracks.count.as("tracks"),
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


  test("map m:n") {
    val query = translator.translate(
        from(Releases)
          .map{ r => (
              r.title,
              r.artists.count.as("artists"),
            )
          }
      )

    assertEquals(query,
      """|SELECT `releases`.`title`, (
         |  SELECT COUNT(*)
         |  FROM `artists`
         |  JOIN `released_by` ON `released_by`.`artist_id` = `artists`.`id`
         |  WHERE `released_by`.`release_id` = `releases`.`id`
         |) AS `artists`
         |FROM `releases`""".stripMargin)
  }


  test("filter m:1") {
    val query = translator.translate(
      from(Tracks)
        .filter(_.release.genre === "Rock")
    )

    assertEquals(query,
      """|SELECT `tracks`.*
         |FROM `tracks`
         |JOIN `releases` ON `tracks`.`release_id` = `releases`.`id`
         |WHERE `releases`.`genre` = 'Rock'""".stripMargin)
  }


  test("filter 1:m") {
    val query = translator.translate(
        from(Releases)
          .filter(_.tracks.count > 5)
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


  test("filter m:n") {
    val query = translator.translate(
        from(Releases)
          .filter(_.artists.count > 5)
          .map(_.title)
      )

    assertEquals(query,
      """|SELECT `releases`.`title`
         |FROM `releases`
         |WHERE (
         |  SELECT COUNT(*)
         |  FROM `artists`
         |  JOIN `released_by` ON `released_by`.`artist_id` = `artists`.`id`
         |  WHERE `released_by`.`release_id` = `releases`.`id`
         |) > 5""".stripMargin)
  }


  test("sort m:1") {
    val query = translator.translate(
      from(Tracks)
        .sortBy(_.release.country)
    )

    assertEquals(query,
      """|SELECT `tracks`.*
         |FROM `tracks`
         |JOIN `releases` ON `tracks`.`release_id` = `releases`.`id`
         |ORDER BY `releases`.`country`""".stripMargin)
  }


  test("sort 1:m") {
    val query = translator.translate(
        from(Releases)
          .sortBy(_.tracks.count.desc)
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


  test("sort m:n") {
    val query = translator.translate(
        from(Releases)
          .sortBy(_.artists.count.desc)
          .map(_.title)
      )

    assertEquals(query,
      """|SELECT `releases`.`title`
         |FROM `releases`
         |ORDER BY (
         |  SELECT COUNT(*)
         |  FROM `artists`
         |  JOIN `released_by` ON `released_by`.`artist_id` = `artists`.`id`
         |  WHERE `released_by`.`release_id` = `releases`.`id`
         |) DESC""".stripMargin)
  }


  test("sort m:1:m") {
    val query = translator.translate(
      from(Tracks)
        .sortBy(_.release.tracks.count.desc)
    )

    assertEquals(query,
      """|SELECT `tracks_1`.*
         |FROM `tracks` `tracks_1`
         |JOIN `releases` ON `tracks_1`.`release_id` = `releases`.`id`
         |ORDER BY (
         |  SELECT COUNT(*)
         |  FROM `tracks` `tracks_2`
         |  WHERE `tracks_2`.`release_id` = `releases`.`id`
         |) DESC""".stripMargin)
  }


  test("subquery on the same table") {
    val query = translator.translate(
      from(Releases).map { r => (
          r.title,
          from(Releases).filter(_.id < r.id).count.as("preceding"),
        )
      }
    )

    assertEquals(query,
      """|SELECT `releases_1`.`title`, (
         |  SELECT COUNT(*)
         |  FROM `releases` `releases_2`
         |  WHERE `releases_2`.`id` < `releases_1`.`id`
         |) AS `preceding`
         |FROM `releases` `releases_1`""".stripMargin)
  }


  test("exists") {
    val query = translator.translate(
      from(Releases).filter(_.artists.exists(_.name === "Radiohead"))
    )

    assertEquals(query,
      """|SELECT `releases`.*
         |FROM `releases`
         |WHERE EXISTS (
         |  SELECT 1
         |  FROM `artists`
         |  JOIN `released_by` ON `released_by`.`artist_id` = `artists`.`id`
         |  WHERE `released_by`.`release_id` = `releases`.`id` AND `artists`.`name` = 'Radiohead'
         |)""".stripMargin)
  }


  test("simple flatMap") {
    val query = translator.translate(
      from(Releases).flatMap{ r => r.artists.map{ a => (a.name, r.title) } }
    )

    assertEquals(query,
      """|SELECT `artists`.`name`, `releases`.`title`
         |FROM
         |  `releases`,
         |  `artists`
         |    JOIN `released_by` ON `released_by`.`artist_id` = `artists`.`id`
         |WHERE `released_by`.`release_id` = `releases`.`id`""".stripMargin)
  }


  test("complex flatMap") {
    val query = translator.translate(
      from(Artists)
        .filter(_.releases.flatMap(_.tracks).map(_.duration).sum > 10000)
    )

    assertEquals(query,
      """|SELECT `artists`.*
         |FROM `artists`
         |WHERE SUM(
         |  SELECT `tracks`.`duration`
         |  FROM
         |    `releases`
         |      JOIN `released_by` ON `released_by`.`release_id` = `releases`.`id`,
         |    `tracks`
         |  WHERE `released_by`.`artist_id` = `artists`.`id` AND `tracks`.`release_id` = `releases`.`id`
         |) > 10000""".stripMargin)
  }
