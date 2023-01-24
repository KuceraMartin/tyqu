package tyqu.execution

import java.sql.*

import tyqu.*
import tyqu.translators.*


class QueryExecutor(connection: Connection, translator: Translator):

	def execute[R](conversion: ResultSet => R, q: SqlQuery): Iterator[R] =
		val stmt = connection.prepareStatement(q.query).nn
		for (param, i) <- q.parameters.zipWithIndex do
			val j = i + 1
			param match
				case v: Int => stmt.setInt(j, v)
				case v: Double => stmt.setDouble(j, v)
				case _ => stmt.setString(j, param.toString)
		val rs = stmt.executeQuery().nn
		ResultIterator(rs, conversion)

	def execute[S <: Scope](qb: QueryBuilder[S])(using ref: RefinedResult[S]): Iterator[ref.Refined] =
		val conversion = QueryExecutor.scopeToConversion(qb.scope)
		execute(conversion, translator.translate(qb))
		

end QueryExecutor


object QueryExecutor:

	def tupleConversion[R](columns: Seq[String])(rs: ResultSet): R =
		val m = columns.map{ c =>
				(c -> rs.getObject(c))
			}
			.toMap
		Result(m).asInstanceOf[R]

	def singletonConversion[R](rs: ResultSet): R =
		rs.getObject(1).asInstanceOf[R]


	def scopeToConversion[S <: Scope](scope: S)(using ref: RefinedResult[S]): ResultSet => ref.Refined =
		scope match
			case s: MultiScope =>
				tupleConversion[ref.Refined](s.toList.map(_.alias))
			case e: AnyExpression =>
				singletonConversion[ref.Refined]


class MySqlQueryExecutor(connection: Connection) extends QueryExecutor(connection, MySqlTranslator)
class PostgreSqlQueryExecutor(connection: Connection) extends QueryExecutor(connection, PostgreSqlTranslator)
