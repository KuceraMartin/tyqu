package tyqu.translators

import tyqu.QueryBuilder


case class SqlQuery(query: String, parameters: Seq[Any])


trait Translator:

	def translate(qb: QueryBuilder[?]): SqlQuery
