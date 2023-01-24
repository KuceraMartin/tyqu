//> using lib "org.postgresql:postgresql:42.5.1"

package tyqu_demo

import java.sql.DriverManager

import tyqu.{*, given}
import tyqu.execution.QueryExecutor
import tyqu.translators.Translator
import tyqu.platforms.*
import tyqu.execution.PostgreSqlQueryExecutor
import tyqu.execution.RefinedResult
import tyqu.translators.PostgreSqlTranslator

import Schema.*


@main
def main =
  val connection = DriverManager.getConnection("jdbc:postgresql://localhost:5432/discogs?user=postgres&password=BigData1&ssl=false").nn
  given translator: Translator = PostgreSqlTranslator
  given QueryExecutor(connection, translator)

  val q = Queries.releasesByArtistName
  q.execute("ABBA").foreach{ r =>
    println(r.title)
  }

