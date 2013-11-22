package de.choffmeister.secpwd

import java.util.Date
import java.util.UUID.randomUUID
import org.rogach.scallop._

object Main {
  def main(args: Array[String]): Unit = {
    execute(new CommandLineInterface(args))
  }

  def execute(cli: CommandLineInterface): Unit = cli.subcommand match {
    case _ =>
      cli.printHelp()
  }

  class CommandLineInterface(val arguments: Seq[String]) extends ScallopConf(arguments) {
    version(s"secpwd v0.0.0 (c) 2013 Christian Hoffmeister <mail@choffmeister.de>")
  }
}
