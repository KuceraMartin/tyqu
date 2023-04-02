package tyqu.translators

import scala.language.implicitConversions

import tyqu.platforms.MySqlPlatform
import tyqu.{*, given}


class GenericSqlTranslatorSubqueriesTest extends UnitTest:

  object Releases extends Table:
    val id = Column[Int](primary = true)
    val title = Column[String]()
    val country = Column[String]()
    val genre = Column[String]()
    lazy val artists = ManyToMany(Artists, ReleasedBy, ReleasedBy.releaseId, ReleasedBy.artistId)
    lazy val tracks = OneToMany(Tracks, Tracks.release)

  object Tracks extends Table:
    val id = Column[Int](primary = true)
    val position = Column[String]()
    val title = Column[String]()
    val duration = Column[Int]()
    val releaseId = Column[Int | Null]()
    lazy val release = ManyToOne(Releases, releaseId, nullable = true)
  
  object Artists extends Table:
    val id = Column[Int](primary = true)
    val name = Column[String]()
    val releases = ManyToMany(Releases, ReleasedBy, ReleasedBy.artistId, ReleasedBy.releaseId)
  
  object ReleasedBy extends Table:
    val releaseId = Column[Int]()
    val artistId = Column[Int]()

  val translator = new GenericSqlTranslator(MySqlPlatform)


  test("map m:1") {
    val query = translator.translate(
      from(Tracks)
        .map{ t => (t.title, t.release.genre) }
    )

    assertEquals(query,
      SqlQuery(
        """|SELECT `tracks`.`title`, `releases`.`genre`
           |FROM `tracks`
           |LEFT JOIN `releases` ON `releases`.`id` = `tracks`.`release_id`""".stripMargin,
        Seq.empty
      ))
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
      SqlQuery(
        """|SELECT `releases`.`title`, (
           |  SELECT COUNT(*)
           |  FROM `tracks`
           |  WHERE `tracks`.`release_id` = `releases`.`id`
           |) AS `tracks`
           |FROM `releases`""".stripMargin,
        Seq.empty
      ))
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
      SqlQuery(
        """|SELECT `releases`.`title`, (
           |  SELECT COUNT(*)
           |  FROM `artists`
           |  JOIN `released_by` ON `released_by`.`artist_id` = `artists`.`id`
           |  WHERE `released_by`.`release_id` = `releases`.`id`
           |) AS `artists`
           |FROM `releases`""".stripMargin,
        Seq.empty
      ))
  }


  test("filter m:1") {
    val query = translator.translate(
      from(Tracks)
        .filter(_.release.genre === "Rock")
    )

    assertEquals(query,
      SqlQuery(
        """|SELECT `tracks`.*
           |FROM `tracks`
           |LEFT JOIN `releases` ON `releases`.`id` = `tracks`.`release_id`
           |WHERE `releases`.`genre` = ?""".stripMargin,
        List("Rock")
      ))
  }


  test("filter 1:m") {
    val query = translator.translate(
        from(Releases)
          .filter(_.tracks.count > 5)
          .map(_.title)
      )

    assertEquals(query,
        SqlQuery(
        """|SELECT `releases`.`title`
           |FROM `releases`
           |WHERE (
           |  SELECT COUNT(*)
           |  FROM `tracks`
           |  WHERE `tracks`.`release_id` = `releases`.`id`
           |) > ?""".stripMargin,
        List(5)
        ))
  }


  test("filter m:n") {
    val query = translator.translate(
        from(Releases)
          .filter(_.artists.count > 5)
          .map(_.title)
      )

    assertEquals(query,
      SqlQuery(
        """|SELECT `releases`.`title`
           |FROM `releases`
           |WHERE (
           |  SELECT COUNT(*)
           |  FROM `artists`
           |  JOIN `released_by` ON `released_by`.`artist_id` = `artists`.`id`
           |  WHERE `released_by`.`release_id` = `releases`.`id`
           |) > ?""".stripMargin,
        List(5)
      ))
  }


  test("sort m:1") {
    val query = translator.translate(
      from(Tracks)
        .sortBy(_.release.country)
    )

    assertEquals(query,
      SqlQuery(
        """|SELECT `tracks`.*
           |FROM `tracks`
           |LEFT JOIN `releases` ON `releases`.`id` = `tracks`.`release_id`
           |ORDER BY `releases`.`country`""".stripMargin,
        Seq.empty
      ))
  }


  test("sort 1:m") {
    val query = translator.translate(
        from(Releases)
          .sortBy(_.tracks.count.desc)
          .map(_.title)
      )

    assertEquals(query,
      SqlQuery(
        """|SELECT `releases`.`title`
           |FROM `releases`
           |ORDER BY (
           |  SELECT COUNT(*)
           |  FROM `tracks`
           |  WHERE `tracks`.`release_id` = `releases`.`id`
           |) DESC""".stripMargin,
        Seq.empty
      ))
  }


  test("sort m:n") {
    val query = translator.translate(
        from(Releases)
          .sortBy(_.artists.count.desc)
          .map(_.title)
      )

    assertEquals(query,
      SqlQuery(
        """|SELECT `releases`.`title`
           |FROM `releases`
           |ORDER BY (
           |  SELECT COUNT(*)
           |  FROM `artists`
           |  JOIN `released_by` ON `released_by`.`artist_id` = `artists`.`id`
           |  WHERE `released_by`.`release_id` = `releases`.`id`
           |) DESC""".stripMargin,
        Seq.empty
      ))
  }


  test("sort m:1:m") {
    val query = translator.translate(
      from(Tracks)
        .sortBy(_.release.tracks.count.desc)
    )

    assertEquals(query,
      SqlQuery(
        """|SELECT `tracks_1`.*
           |FROM `tracks` `tracks_1`
           |LEFT JOIN `releases` ON `releases`.`id` = `tracks_1`.`release_id`
           |ORDER BY (
           |  SELECT COUNT(*)
           |  FROM `tracks` `tracks_2`
           |  WHERE `tracks_2`.`release_id` = `releases`.`id`
           |) DESC""".stripMargin,
        Seq.empty
      ))
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
      SqlQuery(
        """|SELECT `releases_1`.`title`, (
           |  SELECT COUNT(*)
           |  FROM `releases` `releases_2`
           |  WHERE `releases_2`.`id` < `releases_1`.`id`
           |) AS `preceding`
           |FROM `releases` `releases_1`""".stripMargin,
        Seq.empty
      ))
  }


  test("exists") {
    val query = translator.translate(
      from(Releases).filter(_.artists.exists(_.name === "Radiohead"))
    )

    assertEquals(query,
      SqlQuery(
        """|SELECT `releases`.*
           |FROM `releases`
           |WHERE EXISTS (
           |  SELECT 1
           |  FROM `artists`
           |  JOIN `released_by` ON `released_by`.`artist_id` = `artists`.`id`
           |  WHERE `released_by`.`release_id` = `releases`.`id` AND `artists`.`name` = ?
           |)""".stripMargin,
        List("Radiohead")
      ))
  }


  test("simple flatMap") {
    val query = translator.translate(
      from(Releases).flatMap{ r => r.artists.map{ a => (a.name, r.title) } }
    )

    assertEquals(query,
      SqlQuery(
        """|SELECT `artists`.`name`, `releases`.`title`
           |FROM
           |  `releases`,
           |  `artists`
           |    JOIN `released_by` ON `released_by`.`artist_id` = `artists`.`id`
           |WHERE `released_by`.`release_id` = `releases`.`id`""".stripMargin,
        Seq.empty
      ))
  }


  test("complex flatMap") {
    val query = translator.translate(
      from(Artists)
        .filter(_.releases.flatMap(_.tracks).map(_.duration).sum > 10000)
    )

    assertEquals(query,
      SqlQuery(
        """|SELECT `artists`.*
           |FROM `artists`
           |WHERE (
           |  SELECT SUM(`tracks`.`duration`)
           |  FROM
           |    `releases`
           |      JOIN `released_by` ON `released_by`.`release_id` = `releases`.`id`,
           |    `tracks`
           |  WHERE `released_by`.`artist_id` = `artists`.`id` AND `tracks`.`release_id` = `releases`.`id`
           |) > ?""".stripMargin,
        List(10000)
      ))
  }


  test("m:1 nullable makes all columns in the joined table nullable") {
    val query = translator.translate(
      from(Tracks).map{ t => (t.title, t.release.title.getOrElse("no release").as("release")) }
    )

    assertEquals(query,
      SqlQuery(
        """|SELECT `tracks`.`title`, COALESCE(`releases`.`title`, ?) AS `release`
           |FROM `tracks`
           |LEFT JOIN `releases` ON `releases`.`id` = `tracks`.`release_id`""".stripMargin,
        List("no release")
      ))
  }


  test("for comprehension") {
    val query = translator.translate(
      for
        a <- from(Artists) if a.name === "Daft Punk"
        r <- a.releases
        t <- r.tracks
      yield (
        a.name,
        r.title.as("release"),
        t.title.as("track"),
      )
    )

    assertEquals(query,
      SqlQuery(
        """|SELECT `artists`.`name`, `releases`.`title` AS `release`, `tracks`.`title` AS `track`
           |FROM
           |  `artists`,
           |  `releases`
           |    JOIN `released_by` ON `released_by`.`release_id` = `releases`.`id`,
           |  `tracks`
           |WHERE `artists`.`name` = ? AND (`released_by`.`artist_id` = `artists`.`id` AND `tracks`.`release_id` = `releases`.`id`)""".stripMargin,
        List("Daft Punk")
      ))
  }
