package tyqu_demo

import tyqu.{*, given}

object Schema:

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

    lazy val release = ManyToOne(Releases, releaseId, nullable = true)


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
