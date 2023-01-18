//> using lib "org.postgresql:postgresql:42.5.1"

package tyqu_demo

import java.sql.DriverManager

import tyqu.{*, given}
import tyqu.execution.QueryExecutor
import tyqu.translators.GenericSqlTranslator
import tyqu.platforms.*
import tyqu.execution.PostgreSqlQueryExecutor
import tyqu.execution.RefinedResult


case object Releases extends Table:
  val id = Column[Int](primary = true)
  val title = Column[String]()
  val country = Column[String]()
  val genre = Column[String]()

  lazy val artists = ManyToMany(Artists, ReleasedBy, ReleasedBy.releaseId, ReleasedBy.artistId)
  lazy val tracks = OneToMany(Tracks, Tracks.release)


case object Tracks extends Table:
  val id = Column[Int](primary = true)
  val releaseId = Column[Int]()
  val position = Column[String]()
  val title = Column[String]()
  val duration = Column[Int]()

  lazy val release = ManyToOne(Releases, releaseId)


case object Artists extends Table:
  val id = Column[Int](primary = true)
  val name = Column[String]()
  val realName = Column[String]()
  val profile = Column[String]()
  val url = Column[String]

  lazy val releases = ManyToMany(Releases, ReleasedBy, ReleasedBy.artistId, ReleasedBy.releaseId)


case object ReleasedBy extends Table:
  val releaseId = Column[Int]()
  val artistId = Column[Int]()


object Demo:
  def main(args: Array[String]) =
    val connection = DriverManager.getConnection("jdbc:postgresql://localhost:5432/discogs?user=postgres&password=BigData1&ssl=false").nn
    given PostgreSqlQueryExecutor(connection)

    val q =
      // Find out the average track duration.

      // List the titles of all releases by Radiohead that contain less than 5 tracks, sorted in alphabetical order.

      // What are the names and IDs of the top 10 artists with the most releases?

      // How many artists have at least 10000 seconds of released music (i.e., total track duration >= 10000) and at least one release with the genre 'Classical'?
      from(Releases).limit(10).map(_.id * 2)

    // println(translator.translate(q))

    q.execute().foreach(println)