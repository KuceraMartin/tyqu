package tyqu.platforms


trait Platform:

  def formatIdentifier(value: String): String

  def formatStringLiteral(value: String): String =
    f"'$value'"

  def isInfixOperator(functionName: String): Boolean =
    Set(
      "<",
      "<=",
      ">",
      ">=",
      "=",
      "!=",
    ).contains(functionName)
