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
import de.choffmeister.securestring.SecureString

class Main(val directory: File) {
  if (!directory.exists()) directory.mkdirs()

  def init(passphrase: SecureString): Database = {
    val db = Database.create()
    Database.serialize(passphrase, path(db.id), db)
    head = db.id
    db
  }

  def show(passphrase: SecureString, id: String): Option[PasswordEntry] = {
    val db = Database.deserialize(passphrase, path(head))
    db.passwords.find(_.id == id)
  }
  
  def list(passphrase: SecureString): List[PasswordEntry] = {
    val db = Database.deserialize(passphrase, path(head))
    db.passwords
  }

  def add(passphrase: SecureString, id: String, name: String, password: Either[SecureString, (PasswordCharacters, Int)]): PasswordEntry = {
    val db1 = Database.deserialize(passphrase, path(head))
    val pwd = password match {
      case Left(pwd) => pwd
      case Right((chars, len)) => PasswordUtils.generate(len, chars)
    }
    val entry = PasswordEntry(id, new Date(), name, pwd)
    val db2 = Database.addPassword(db1, entry)
    Database.serialize(passphrase, path(db2.id), db2)
    head = db2.id
    entry
  }

  def remove(passphrase: SecureString, id: String) {
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
      val main = new Main(directory)
      val cli = new CommandLineInterface(args)
      cli.subcommand match {
        case Some(cli.init) =>
          passphrase(main.init(_))
        case Some(cli.list) =>
          for (pwd <- passphrase(main.list(_))) {
            println(s"[${pwd.id}] ${pwd.name}")
          }
        case Some(cli.show) =>
          passphrase(main.show(_, cli.show.id())) match {
            case Some(pwd) =>
              println(pwd.id)
              println(pwd.timeStamp)
              println(pwd.name)
              println(pwd.userName)
              if (cli.show.printPassword()) {
                pwd.password.read(_.foreach(print(_)))
                println()
              }
            case _ =>
              println(s"Cannot find password ${cli.show.id()}")
          }
        case Some(cli.add) =>
          val name = InteractiveConsole.read("Name").get
          val id = InteractiveConsole.readWithDefault("Key", name.toLowerCase)
          val pwdInteractive = InteractiveConsole.readSecureString("Password")
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
          passphrase(main.add(_, id, name, pwd))
        case _ =>
          cli.printHelp()
      }
    } catch {
      case e: Throwable => println(e.getMessage)
    }
  }

  def passphrase[T](inner: SecureString => T): T = InteractiveConsole.readSecureString("Passphrase") match {
    case Some(pp) =>
      try {
        inner(pp)
      } finally {
        pp.wipe()
      }
    case _ => passphrase(inner)
  }

  class CommandLineInterface(val arguments: Seq[String]) extends ScallopConf(arguments) {
    version(s"secpwd v0.0.1 (c) 2013 Christian Hoffmeister <mail@choffmeister.de>")

    val init = new Subcommand("init")
    val list = new Subcommand("list")
    val show = new Subcommand("show") {
      val id = trailArg[String]("id")
      val printPassword = opt[Boolean]("print-password", 'p')
    }
    val add = new Subcommand("add")
    val remove = new Subcommand("rm") {
      val id = trailArg[String]("id")
    }
  }
}
