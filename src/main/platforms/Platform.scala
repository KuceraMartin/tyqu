package tyqu.platforms


trait Platform:

  def formatIdentifier(value: String): String
