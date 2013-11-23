package de.choffmeister.secpwd

import java.util.Date
import java.util.UUID.randomUUID
import java.io.File
import org.rogach.scallop._
import scala.language.reflectiveCalls
import de.choffmeister.secpwd.security.PasswordUtils
import de.choffmeister.secpwd.security.PasswordCharacters
import de.choffmeister.secpwd.utils.InteractiveConsole

object Main {
  val path = new File(System.getProperty("user.home"), ".secpwd")

  def main(args: Array[String]): Unit = {
    try {
      execute(new CommandLineInterface(args))
    } catch {
      case e: Throwable => println(e.getMessage)
    }
  }

  def getPassphrase(): Array[Char] = InteractiveConsole.readStringSecure("Passphrase") match {
    case Some(pp) => pp
    case _ => getPassphrase()
  }

  def execute(cli: CommandLineInterface): Unit = cli.subcommand match {
    case Some(cli.init) =>
      val pp = getPassphrase()
      val db = Database.create()
      Database.serialize(pp, path, db)
    case Some(cli.list) =>
      val pp = getPassphrase()
      val db = Database.deserialize(pp, path)
      for (pwd <- db.passwords) {
        println(pwd.id)
      }
    case Some(cli.show) =>
      val pp = getPassphrase()
      val db = Database.deserialize(pp, path)
      db.passwords.find(_.id == cli.show.id()) match {
        case Some(pwd) =>
          println(pwd.id)
          println(pwd.timeStamp)
          println(pwd.name)
          println(pwd.password)
          println(pwd.userName)
        case _ =>
          println("Unknown password id")
      }
    case Some(cli.add) =>
      val pp = getPassphrase()
      val db1 = Database.deserialize(pp, path)
      val name = InteractiveConsole.read("Name")
      val id = InteractiveConsole.readWithDefault("Key", nameToId(name.get))
      val pwdInteractive = InteractiveConsole.readStringSecure("Password")
      val pwd = pwdInteractive match {
        case Some(pwd) =>
          pwd
        case _ =>
          val length = InteractiveConsole.readWithDefault[Int]("Password length", 32, _.toInt)
          val alphaLower = InteractiveConsole.readWithDefault[Boolean]("Use lower alpha characters?", true, _.toBoolean)
          val alphaUpper = InteractiveConsole.readWithDefault[Boolean]("Use upper alpha characters?", true, _.toBoolean)
          val numbers = InteractiveConsole.readWithDefault[Boolean]("Use number characters?", true, _.toBoolean)
          val special = InteractiveConsole.readWithDefault[Boolean]("Use special characters?", true, _.toBoolean)
          PasswordUtils.generate(length, PasswordCharacters(alphaLower, alphaUpper, numbers, special))
      }
      val db2 = Database.addPassword(db1, PasswordEntry(id, new Date(), name.get, pwd.mkString("")))
      Database.serialize(pp, path, db2)
    case Some(cli.remove) =>
      val pp = getPassphrase()
      val db1 = Database.deserialize(pp, path)
      val db2 = Database.removePasswordById(db1, cli.remove.id())
      Database.serialize(pp, path, db2)
    case _ =>
      cli.printHelp()
  }

  def nameToId(name: String) = name.toLowerCase

  class CommandLineInterface(val arguments: Seq[String]) extends ScallopConf(arguments) {
    version(s"secpwd v0.0.0 (c) 2013 Christian Hoffmeister <mail@choffmeister.de>")

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
