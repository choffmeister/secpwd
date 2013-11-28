package de.choffmeister.secpwd

import java.util.Date
import java.util.UUID
import java.io.{File, FileNotFoundException}
import org.rogach.scallop._
import scala.language.reflectiveCalls
import de.choffmeister.secpwd.security.PasswordUtils
import de.choffmeister.secpwd.security.PasswordCharacters
import de.choffmeister.secpwd.utils.CommandLineInterface
import de.choffmeister.secpwd.utils.NativeCommandLineInterface
import de.choffmeister.secpwd.utils.RichFile._
import de.choffmeister.secpwd.utils.Clipboard
import de.choffmeister.securestring.SecureString

class CommandLineArguments(val arguments: Seq[String]) extends ScallopConf(arguments) {
  version(s"secpwd v0.0.2 (c) 2013 Christian Hoffmeister <mail@choffmeister.de>")

  val init = new Subcommand("init")
  val list = new Subcommand("list")
  val history = new Subcommand("history")
  val show = new Subcommand("show") {
    val idOrKey = trailArg[String]("id or key")
    val printPassword = opt[Boolean]("print-password", 'p')
  }
  val get = new Subcommand("get") {
    val idOrKey = trailArg[String]("id or key")
  }
  val add = new Subcommand("add") {
    val key = trailArg[String]("key")
  }
  val remove = new Subcommand("remove") {
    val idOrKey = trailArg[String]("id or key")
  }
  val renew = new Subcommand("renew") {
    val key = trailArg[String]("key")
  }
  val sync = new Subcommand("sync")
}

class Main(val directory: File, val cli: CommandLineInterface, val config: Config = Config()) {
  if (!directory.exists()) directory.mkdirs()

  def hasCurrent: Boolean = {
    try {
      head
      true
    } catch {
      case e: FileNotFoundException => false
      case e: IllegalArgumentException => false
    }
  }
  def current(passphrase: SecureString): Database = Database.deserialize(passphrase, currentRaw)
  def setCurrent(db: Database, passphrase: SecureString): Unit = {
    path(db.id).bytes = Database.serialize(passphrase, db)
    setHead(db.id)
  }
  def currentRaw: Array[Byte] = path(head).bytes
  def head: UUID = UUID.fromString(new File(directory, "HEAD").text.trim)
  def setHead(id: UUID): Unit = new File(directory, "HEAD").text = id.toString
  def path(id: UUID): File = new File(directory, id.toString)

  def init(passphrase: SecureString) = InitCommand.headless(this, passphrase)
  def add(passphrase: SecureString, key: String, password: Either[SecureString, (PasswordCharacters, Int)], name: String, description: String, userName: String, url: String, customFields: Map[String, String]) = AddCommand.headless(this, key, password, name, description, userName, url, customFields, passphrase)
  def list(passphrase: SecureString) = ListCommand.headless(this, passphrase)
  def show(passphrase: SecureString, idOrKey: String) = ShowCommand.headless(this, idOrKey, passphrase)
  def remove(passphrase: SecureString, key: String) = RemoveCommand.headless(this, key, passphrase)

  def run(args: Array[String]): Unit = {
    val cla = new CommandLineArguments(args)

    cla.subcommand match {
      case Some(cla.init) => InitCommand(this)
      case Some(cla.list) => passphrase(ListCommand(this, _))
      case Some(cla.history) => passphrase(HistoryCommand(this, _))
      case Some(cla.show) => passphrase(ShowCommand(this, cla.show.idOrKey(), cla.show.printPassword(), _))
      case Some(cla.get) => passphrase(GetCommand(this, cla.get.idOrKey(), _))
      case Some(cla.add) => passphrase(AddCommand(this, cla.add.key(), _))
      case Some(cla.remove) => passphrase(RemoveCommand(this, cla.remove.idOrKey(), _))
      case Some(cla.renew) => passphrase(RenewCommand(this, cla.renew.key(), _))
      case Some(cla.sync) => passphrase(SyncCommand(this, _))
      case _ =>
        // TODO print help on cli object
        cla.printHelp()
    }
  }

  def passphrase[T](inner: SecureString => T): T = cli.readSecureString("Passphrase") match {
    case Some(pp) =>
      try {
        inner(pp)
      } finally {
        pp.wipe()
      }
    case _ => throw new Exception("You must provide a passphrase")
  }
}

object Main {
  lazy val directory = Option(System.getProperty("secpwd.database")) match {
    case Some(dir) => new File(dir)
    case _ => new File(System.getProperty("user.home"), ".secpwd")
  }

  def main(args: Array[String]): Unit = {
    val cli = new NativeCommandLineInterface()
    val conf = Config.load(directory)
    val main = new Main(directory, cli, conf)
    try {
      main.run(args)
      System.exit(0)
    } catch {
      case e: Throwable =>
      cli.printError(e.getMessage)
      System.exit(1)
    }
  }
}
