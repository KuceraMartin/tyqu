package tyqu.execution

import java.sql.*

import tyqu.*
import tyqu.translators.*


class QueryExecutor(connection: Connection, translator: Translator):
	
	def execute[S <: Scope](qb: QueryBuilder[S])(using ref: RefinedResult[S]): Iterator[ref.Refined] =
		val query = translator.translate(qb)
		val stmt = connection.createStatement().nn
		val rs = stmt.executeQuery(query).nn
		qb.scope match
			case s: MultiScope =>
				def conversion(rs: ResultSet) =
					val m = s.toList.map{ e =>
						(e.alias.toString -> rs.getString(e.alias))
					}
					.toMap
					Result(m).asInstanceOf[ref.Refined]
				ResultIterator(rs, conversion)
			case e: Expression[?] =>
				ResultIterator(rs, _.getString(1).asInstanceOf[ref.Refined])

end QueryExecutor


class MySqlQueryExecutor(connection: Connection) extends QueryExecutor(connection, MySqlTranslator)
class PostgreSqlQueryExecutor(connection: Connection) extends QueryExecutor(connection, PostgreSqlTranslator)