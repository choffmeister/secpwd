package de.choffmeister.secpwd.utils

import de.choffmeister.securestring.SecureString
import java.io.StringWriter
import scala.collection.mutable.Queue

class NullCommandLineInterface extends CommandLineInterface {
  def read[T](label: String, conv: String => T): Option[T] = None
  def readWithDefault[T](label: String, default: T, conv: String => T): T = default
  def readSecureString(label: String): Option[SecureString] = None
  def readSecureStringWithRepitition(label: String): Option[SecureString] = None
  def print(content: Any) = {}
  def println(content: Any) = {}
  def printInfo(label: String, message: String) = {}
  def printSuccess(message: String) = {}
  def printError(message: String) = {}
}

class MockCommandLineInterface extends CommandLineInterface {
  private val q = Queue.empty[Option[String]]
  val out = new StringWriter()
  override def toString() = out.toString

  def queueInput(input: Option[String]) = q.enqueue(input)

  def read[T](label: String, conv: String => T): Option[T] = q.dequeue() match {
    case Some(v) =>
      println(formatLabel(label) + v)
      Some(conv(v))
    case _ =>
      println(formatLabel(label))
      None
  }

  def readWithDefault[T](label: String, default: T, conv: String => T): T = q.dequeue() match {
    case Some(v) =>
      println(formatLabelWithDefault(label, default) + v)
      conv(v)
    case _ =>
      println(formatLabelWithDefault(label, default))
      default
  }

  def readSecureString(label: String): Option[SecureString] = q.dequeue() match {
    case Some(v) =>
      println(formatLabel(label) + s"{{$v}}")
      Some(SecureString(v.toCharArray))
    case _ =>
      println(formatLabel(label))
      None
  }

  def readSecureStringWithRepitition(label: String): Option[SecureString] = {
    readSecureString(label) match {
      case Some(ss1) =>
        readSecureString("Repeat") match {
          case Some(ss2) if ss1 == ss2 => Some(ss1)
          case _ => throw new Exception("Repitition did not match")
        }
      case _ => None
    }
  }

  def print(content: Any) = out.write(s"$content")
  def println(content: Any) = out.write(s"$content\n")
  def printInfo(label: String, message: String) = out.write(s"[${label}] $message\n")
  def printSuccess(message: String) = out.write(s"[success] $message\n")
  def printError(message: String) = out.write(s"[error] $message\n")

  private def formatLabel(label: String) = s"$label: "
  private def formatLabelWithDefault(label: String, default: Any) = s"$label [$default]: "
}
