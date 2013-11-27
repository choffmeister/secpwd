package de.choffmeister.secpwd.utils

import de.choffmeister.securestring.SecureString

trait CommandLineInterface {
  def read[T](label: String, conv: String => T): Option[T]
  def readWithDefault[T](label: String, default: T, conv: String => T): T
  def readSecureString(label: String): Option[SecureString]

  def read(label: String): Option[String] = read(label, s => s)
  def readWithDefault(label: String, default: String): String = readWithDefault(label, default, s => s)

  def print(content: Any)
  def println(content: Any)
  def printInfo(label: String, message: String)
  def printSuccess(message: String)
  def printError(message: String)
}

class NativeCommandLineInterface extends CommandLineInterface {
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

  def readSecureString(label: String): Option[SecureString] = {
    val console = Option(System.console())
    console match {
      case Some(c) => c.readPassword(formatLabel(label)) match {
        case ss if ss.length > 0 => Some(SecureString(ss))
        case _ => None
      }
      case _ => throw new Exception("No console available")
    }
  }

  def print(content: Any) = System.out.print(content)
  def println(content: Any) = System.out.print(content)
  def printInfo(label: String, message: String) = println(s"[${Console.BLUE}${label}${Console.RESET}] ${message}")
  def printSuccess(message: String) = println(s"[${Console.GREEN}success${Console.RESET}] ${message}")
  def printError(message: String) = println(s"[${Console.RED}error${Console.RESET}] ${message}")

  private def formatLabel(label: String) = s"$label: "
  private def formatLabelWithDefault(label: String, default: Any) = s"$label [$default]: "
}