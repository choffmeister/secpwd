package de.choffmeister.secpwd.utils

import de.choffmeister.securestring.SecureString
import scala.collection.mutable.Queue

class NullCommandLineInterface extends CommandLineInterface {
  def read[T](label: String, conv: String => T): Option[T] = None
  def readWithDefault[T](label: String, default: T, conv: String => T): T = default
  def readSecureString(label: String): Option[SecureString] = None
  def print(content: Any) = {}
  def println(content: Any) = {}
  def printInfo(label: String, message: String) = {}
  def printSuccess(message: String) = {}
  def printError(message: String) = {}
}

class MockCommandLineInterface extends CommandLineInterface {
  private val q = Queue.empty[Option[String]]

  def queueInput(input: Option[String]) = q.enqueue(input)

  def read[T](label: String, conv: String => T): Option[T] = q.dequeue() match {
    case Some(v) => Some(conv(v))
    case _ => None
  }

  def readWithDefault[T](label: String, default: T, conv: String => T): T = q.dequeue() match {
    case Some(v) => conv(v)
    case _ => default
  }

  def readSecureString(label: String): Option[SecureString] = q.dequeue() match {
    case Some(v) => Some(SecureString(v.toCharArray))
    case _ => None
  }

  def print(content: Any) = {}
  def println(content: Any) = {}
  def printInfo(label: String, message: String) = {}
  def printSuccess(message: String) = {}
  def printError(message: String) = {}
}