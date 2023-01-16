package tyqu.translators

import tyqu.QueryBuilder


trait Translator:

	def translate(qb: QueryBuilder[?]): String
