package de.choffmeister.secpwd

import java.util.Date
import java.util.UUID
import java.io.File
import org.rogach.scallop._
import scala.language.reflectiveCalls
import de.choffmeister.secpwd.security.PasswordUtils
import de.choffmeister.secpwd.security.PasswordCharacters
import de.choffmeister.secpwd.utils.InteractiveConsole
import de.choffmeister.secpwd.utils.RichFile._

class Main(val directory: File, passphrase: => Array[Char]) {
  if (!directory.exists()) directory.mkdirs()

  def init(): Database = {
    val db = Database.create()
    Database.serialize(passphrase, path(db.id), db)
    head = db.id
    db
  }

  def show(id: String): Option[PasswordEntry] = {
    val db = Database.deserialize(passphrase, path(head))
    db.passwords.find(_.id == id)
  }
  
  def list(): List[PasswordEntry] = {
    val db = Database.deserialize(passphrase, path(head))
    db.passwords
  }

  def add(id: String, name: String, password: Either[Array[Char], (PasswordCharacters, Int)]): PasswordEntry = {
    val db1 = Database.deserialize(passphrase, path(head))
    val pwd = password match {
      case Left(pwd) => pwd
      case Right((chars, len)) => PasswordUtils.generate(len, chars)
    }
    val entry = PasswordEntry(id, new Date(), name, pwd.mkString(""))
    val db2 = Database.addPassword(db1, entry)
    Database.serialize(passphrase, path(db2.id), db2)
    head = db2.id
    entry
  }

  def remove(id: String) {
    val db1 = Database.deserialize(passphrase, path(head))
    val db2 = Database.removePasswordById(db1, id)
    Database.serialize(passphrase, path(db2.id), db2)
    head = db2.id
  }

  def head: UUID = UUID.fromString(new File(directory, "HEAD").text.trim)
  def head_=(id: UUID): Unit = new File(directory, "HEAD").text = id.toString
  def path(id: UUID): File = new File(directory, id.toString)
}

object Main {
  val directory = new File(System.getProperty("user.home"), ".secpwd")

  def main(args: Array[String]): Unit = {
    try {
      val main = new Main(directory, passphrase)
      val cli = new CommandLineInterface(args)
      cli.subcommand match {
        case Some(cli.init) =>
          main.init()
        case Some(cli.list) =>
          for (pwd <- main.list) {
            println(s"[${pwd.id}] ${pwd.name}")
          }
        case Some(cli.show) =>
          main.show(cli.show.id()) match {
            case Some(pwd) =>
              println(pwd.id)
              println(pwd.timeStamp)
              println(pwd.name)
              println(pwd.password)
              println(pwd.userName)
            case _ =>
              println(s"Cannot find password ${cli.show.id()}")
          }
        case Some(cli.add) =>
          val name = InteractiveConsole.read("Name").get
          val id = InteractiveConsole.readWithDefault("Key", name.toLowerCase)
          val pwdInteractive = InteractiveConsole.readStringSecure("Password")
          val pwd = pwdInteractive match {
            case Some(pwd) =>
              Left(pwd)
            case _ =>
              val length = InteractiveConsole.readWithDefault[Int]("Password length", 32, _.toInt)
              val alphaLower = InteractiveConsole.readWithDefault[Boolean]("Use lower alpha characters?", true, _.toBoolean)
              val alphaUpper = InteractiveConsole.readWithDefault[Boolean]("Use upper alpha characters?", true, _.toBoolean)
              val numbers = InteractiveConsole.readWithDefault[Boolean]("Use number characters?", true, _.toBoolean)
              val special = InteractiveConsole.readWithDefault[Boolean]("Use special characters?", true, _.toBoolean)
              Right((PasswordCharacters(alphaLower, alphaUpper, numbers, special), length))
          }
          main.add(id, name, pwd)
        case _ =>
          cli.printHelp()
      }
    } catch {
      case e: Throwable => println(e.getMessage)
    }
  }

  lazy val passphrase: Array[Char] = InteractiveConsole.readStringSecure("Passphrase") match {
    case Some(pp) => pp
    case _ => passphrase
  }

  class CommandLineInterface(val arguments: Seq[String]) extends ScallopConf(arguments) {
    version(s"secpwd v0.0.1 (c) 2013 Christian Hoffmeister <mail@choffmeister.de>")

    val init = new Subcommand("init")
    val list = new Subcommand("list")
    val show = new Subcommand("show") {
      val id = trailArg[String]("id")
    }
    val add = new Subcommand("add")
    val remove = new Subcommand("rm") {
      val id = trailArg[String]("id")
    }
  }
}
