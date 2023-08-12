package tyqu.execution

import java.sql.*

import tyqu.*
import tyqu.translators.*


class QueryExecutor(connection: Connection, translator: Translator):
	
	transparent inline def execute[S <: Scope](qb: QueryBuilder[S]): Iterator[?] =
		val SqlQuery(query, parameters) = translator.translate(qb)
		val stmt = connection.prepareStatement(query).nn
		for (param, i) <- parameters.zipWithIndex do
			val j = i + 1
			param match
				case v: Int => stmt.setInt(j, v)
				case v: Double => stmt.setDouble(j, v)
				case _ => stmt.setString(j, param.toString)
		val rs = stmt.executeQuery().nn
		RefinedResult.refinedResult[S](rs)

end QueryExecutor


class MySqlQueryExecutor(connection: Connection) extends QueryExecutor(connection, MySqlTranslator)
class PostgreSqlQueryExecutor(connection: Connection) extends QueryExecutor(connection, PostgreSqlTranslator)
