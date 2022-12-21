package tyqu_demo

import tyqu.{*, given}
import tyqu.translators.GenericSqlTranslator
import tyqu.platforms.MySqlPlatform


case object Releases extends Table:
  val id = Column[Int](primary = true)
  val title = Column[String]()
  val country = Column[String]()
  val genre = Column[String]()
  
  val artists = ManyToMany(Artists, ReleasedBy, ReleasedBy.releaseId, ReleasedBy.artistId)
  val tracks = OneToMany(Tracks, Tracks.release)

  
case object Tracks extends Table:
  val id = Column[Int](primary = true)
  val position = Column[String]()
  val title = Column[String]()
  val duration = Column[Int]()
  val releaseId = Column[Int]()
  
  val release = ManyToOne(Releases, releaseId)


case object Artists extends Table:
  val id = Column[Int](primary = true)
  val name = Column[String]()

  val releases = ManyToMany(Releases, ReleasedBy, ReleasedBy.artistId, ReleasedBy.releaseId)

case object ReleasedBy extends Table:
  val releaseId = Column[Int]()
  val artistId = Column[Int]()


object Demo:
  def main(args: Array[String]) =
    val translator = new GenericSqlTranslator(MySqlPlatform)

    val q =
      // average track duration
      from(Tracks).map(_.duration.avg)

      // List the titles of all releases by Radiohead in alphabetical order.
      // from(Releases)
      //   .filter(_.artists.exists(_.name === "Radiohead"))
      //   .map(_.title)
      //   .sorted

      // What are the names and IDs of the top 10 artists with the most releases?
      // from(Artists)
      //   .sortBy(_.releases.count.desc)
      //   .map(a => (a.name, a.id))
      //   .limitBy(10)

    println(translator.translate(q))
