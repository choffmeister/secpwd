package de.choffmeister.secpwd

import java.util.Date
import java.util.UUID.randomUUID
import java.io.File
import org.rogach.scallop._
import scala.language.reflectiveCalls

object Main {
  val path = new File(System.getProperty("user.home"), ".secpwd")

  def main(args: Array[String]): Unit = {
    try {
      execute(new CommandLineInterface(args))
    } catch {
      case e: Throwable => println(e.getMessage)
    }
  }

  def execute(cli: CommandLineInterface): Unit = cli.subcommand match {
    case Some(cli.init) =>
      val db = Database.create()
      Database.serializeFromFile(path, db)
    case Some(cli.show) =>
      val db = Database.deserializeToFile(path)
      println(s"id:        ${db.id}")
      println(s"timestamp: ${db.timeStamp}")
      for (pwd <- db.passwords) {
        println(s"[${pwd.id}] ${pwd.name} -> ${pwd.password}")
      }
    case Some(cli.add) =>
      val db1 = Database.deserializeToFile(path)
      val db2 = Database.addPassword(db1, PasswordEntry(cli.add.id(), new Date(), cli.add.name(), cli.add.password()))
      Database.serializeFromFile(path, db2)
    case Some(cli.remove) =>
      val db1 = Database.deserializeToFile(path)
      val db2 = Database.removePasswordById(db1, cli.remove.id())
      Database.serializeFromFile(path, db2)
    case _ =>
      cli.printHelp()
  }

  class CommandLineInterface(val arguments: Seq[String]) extends ScallopConf(arguments) {
    version(s"secpwd v0.0.0 (c) 2013 Christian Hoffmeister <mail@choffmeister.de>")

    val init = new Subcommand("init")
    val show = new Subcommand("show")
    val add = new Subcommand("add") {
      val id = trailArg[String]("id")
      val name = trailArg[String]("name")
      val password = trailArg[String]("password")
    }
    val remove = new Subcommand("rm") {
      val id = trailArg[String]("id")
    }
  }
}
