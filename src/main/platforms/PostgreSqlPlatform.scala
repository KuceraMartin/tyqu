package tyqu.platforms


object PostgreSqlPlatform extends Platform:

  def formatIdentifier(value: String): String = f"\"$value\""
