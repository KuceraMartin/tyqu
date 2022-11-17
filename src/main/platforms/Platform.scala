package tyqu.platforms


trait Platform:

  def formatIdentifier(value: String): String

  def isInfixOperator(functionName: String): Boolean =
    Set(
      "<",
      "<=",
      ">",
      ">=",
      "=",
      "!=",
    ).contains(functionName)
