package tyqu.execution


class Result(values: Map[String, Any]) extends Selectable:

	def selectDynamic(key: String) = values(key)

end Result
