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
    path(db.id).bytes = Database.serialize(passphrase, db)
    head = db.id
    db
  }

  def show(passphrase: SecureString, idOrKey: String): Option[PasswordEntry] = {
    val db = Database.deserialize(passphrase, path(head).bytes)
    parseIdOrKey(idOrKey) match {
      case Left(id) => db.passwordById(id)
      case Right(key) => db.currentPasswordByKey(key)
    }
  }

  def list(passphrase: SecureString): List[PasswordEntry] = {
    val db = Database.deserialize(passphrase, path(head).bytes)
    db.currentPasswords
  }

  def add(passphrase: SecureString, key: String, password: Either[SecureString, (PasswordCharacters, Int)], name: String, description: String, userName: String, url: String, customFields: Map[String, String]): PasswordEntry = {
    val db1 = Database.deserialize(passphrase, path(head).bytes)
    val pwd = password match {
      case Left(pwd) => pwd
      case Right((chars, len)) => PasswordUtils.generate(len, chars)
    }
    val entry = PasswordEntry(UUID.randomUUID(), new Date(), key, pwd, name, description, userName, url, customFields)
    val db2 = Database.addPassword(db1, entry)
    path(db2.id).bytes = Database.serialize(passphrase, db2)
    head = db2.id
    entry
  }

  def remove(passphrase: SecureString, idOrKey: String) {
    val db1 = Database.deserialize(passphrase, path(head).bytes)
    val db2 = parseIdOrKey(idOrKey) match {
      case Left(id) => Database.removePasswordById(db1, id)
      case Right(key) => Database.removePasswordByKey(db1, key)
    }
    path(db2.id).bytes = Database.serialize(passphrase, db2)
    head = db2.id
  }

  def current(passphrase: SecureString): Database = Database.deserialize(passphrase, path(head).bytes)

  def head: UUID = UUID.fromString(new File(directory, "HEAD").text.trim)
  def head_=(id: UUID): Unit = new File(directory, "HEAD").text = id.toString
  def path(id: UUID): File = new File(directory, id.toString)

  private def parseIdOrKey(idOrKey: String): Either[UUID, String] = {
    try {
      Left(UUID.fromString(idOrKey))
    } catch {
      case e: IllegalArgumentException => Right(idOrKey)
    }
  }
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
          printSuccess("Created new password store")
        case Some(cli.list) =>
          for (pwd <- passphrase(main.list(_)).sortWith(_.key < _.key)) {
            printInfo(pwd.key, s"${pwd.name} (${PasswordUtils.getBitEntropy(pwd.password)} bits) ${pwd.timeStamp}")
          }
        case Some(cli.history) =>
          val db = passphrase(main.current(_))
          for (i <- 0 until db.versions.length - 1) {
            val v1 = db.versions(i + 1)
            val v2 = db.versions(i)
            val diff = Database.diff(db, v1, v2)
            
            println(v2.timeStamp)
            
            for (a <- diff) a match {
              case (key, (None, Some(b))) => printInfo(key, s"Add password (${b.id})")
              case (key, (Some(a), None)) => printInfo(key, s"Remove password (${a.id})")
              case (key, (Some(a), Some(b))) => printInfo(key, s"Update password (${a.id} -> ${b.id})")
              case (_, (None, None)) => throw new Exception("Impossible case")
            }
          }
        case Some(cli.show) =>
          passphrase(main.show(_, cli.show.idOrKey())) match {
            case Some(pwd) =>
              printInfo(pwd.key, "Password information")
              println(s"  Timestamp: ${pwd.timeStamp}")
              println(s"  Name: ${pwd.name}")
              if (pwd.description.length > 0) println(s"  Description: ${pwd.description}")
              if (pwd.userName.length > 0) println(s"  Username: ${pwd.userName}")
              if (pwd.url.length > 0) println(s"  URL: ${pwd.url}")
              println(s"  Strength: ${PasswordUtils.getBitEntropy(pwd.password)} bits")
              if (cli.show.printPassword()) {
                print("  Password: ")
                pwd.password.read(_.foreach(print(_)))
                println()
              } else {
                println("  Password: ***")
              }
            case _ =>
              printError(s"Password ${cli.show.idOrKey()} does not exist")
          }
        case Some(cli.add) =>
          val name = InteractiveConsole.readWithDefault("Name", cli.add.key())
          val description = InteractiveConsole.read("Description").orElse(Some("")).get
          val url = InteractiveConsole.read("URL").orElse(Some("")).get
          val userName = InteractiveConsole.read("Username").orElse(Some("")).get
          val pwdInteractive = InteractiveConsole.readSecureString("Password")
          val pwd = pwdInteractive match {
            case Some(pwd) =>
              InteractiveConsole.readSecureString("Repeat") match {
                case Some(pwdRepeat) if pwd == pwdRepeat => Left(pwd)
                case _ => throw new Exception("Password repetition does not match")
              }
            case _ =>
              val length = InteractiveConsole.readWithDefault[Int]("Password length", 32, _.toInt)
              val alphaLower = InteractiveConsole.readWithDefault[Boolean]("Use lower alpha characters?", true, _.toBoolean)
              val alphaUpper = InteractiveConsole.readWithDefault[Boolean]("Use upper alpha characters?", true, _.toBoolean)
              val numbers = InteractiveConsole.readWithDefault[Boolean]("Use number characters?", true, _.toBoolean)
              val special = InteractiveConsole.readWithDefault[Boolean]("Use special characters?", true, _.toBoolean)
              Right((PasswordCharacters(alphaLower, alphaUpper, numbers, special), length))
          }
          passphrase(main.add(_, cli.add.key(), pwd, name, description, userName, url, Map.empty))
          printSuccess("Added password")
        case Some(cli.remove) =>
          passphrase(main.remove(_, cli.remove.idOrKey()))
          printSuccess("Removed password")
        case _ =>
          cli.printHelp()
      }
    } catch {
      case e: Throwable => printError(e.getMessage)
    }
  }

  def printInfo(label: String, message: String) = println(s"[${Console.BLUE}${label}${Console.RESET}] ${message}")

  def printSuccess(message: String) = println(s"[${Console.GREEN}success${Console.RESET}] ${message}")

  def printError(message: String) = println(s"[${Console.RED}error${Console.RESET}] ${message}")

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
    val history = new Subcommand("history")
    val show = new Subcommand("show") {
      val idOrKey = trailArg[String]("id or key")
      val printPassword = opt[Boolean]("print-password", 'p')
    }
    val add = new Subcommand("add") {
      val key = trailArg[String]("key")
    }
    val remove = new Subcommand("remove") {
      val idOrKey = trailArg[String]("id or key")
    }
  }
}
