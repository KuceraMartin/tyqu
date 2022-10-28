package tyqu.platforms


object MySqlPlatform extends Platform:

  def formatIdentifier(value: String): String = f"`$value`"
