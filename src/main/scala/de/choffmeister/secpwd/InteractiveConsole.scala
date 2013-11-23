package de.choffmeister.secpwd

object InteractiveConsole {
  def read[T](label: String, conv: String => T): Option[T] = {
    print(formatLabel(label))
    readLine() match {
      case str if str.length > 0 => Some(conv(str))
      case _ => None
    }
  }

  def readWithDefault[T](label: String, default: T, conv: String => T): T = {
    print(formatLabelWithDefault(label, default))
    readLine() match {
      case str if str.length > 0 => conv(str)
      case _ => default
    }
  }

  def read(label: String): Option[String] = read(label, s => s)

  def readWithDefault(label: String, default: String): String = readWithDefault(label, default, s => s)

  def readStringSecure(label: String): Option[Array[Char]] = {
    val console = Option(System.console())
    console match {
      case Some(c) => c.readPassword(formatLabel(label)) match {
        case ss if ss.length > 0 => Some(ss)
        case _ => None
      }
      case _ => throw new Exception("No console available")
    }
  }

  private def formatLabel(label: String) = s"$label: "
  private def formatLabelWithDefault(label: String, default: Any) = s"$label [$default]: "
}