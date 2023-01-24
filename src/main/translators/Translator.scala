package tyqu.translators

import tyqu.QueryBuilder


final class RuntimeParameter(val name: String):
  type Type

object RuntimeParameter:
  def apply[T](name: String): RuntimeParameter { type Type = T } =
    new RuntimeParameter(name).asInstanceOf


case class SqlQuery(query: String, parameters: Seq[Any])


trait Translator:

	def translate(qb: QueryBuilder[?]): SqlQuery
