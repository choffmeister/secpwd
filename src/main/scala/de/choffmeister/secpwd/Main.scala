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
  version(s"secpwd v0.0.1 (c) 2013 Christian Hoffmeister <mail@choffmeister.de>")

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

class Main(directory: File, cli: CommandLineInterface, config: Config = Config()) {
  if (!directory.exists()) directory.mkdirs()

  def exists: Boolean = {
    try {
      head
      true
    } catch {
      case e: FileNotFoundException => false
    }
  }

  def init(passphrase: SecureString): Database = {
    if (exists) throw new Exception("Database alreay exists")
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

  def renew(passphrase: SecureString, key: String, password: Either[SecureString, (PasswordCharacters, Int)]) {
    val db1 = Database.deserialize(passphrase, path(head).bytes)
    val pwd = password match {
      case Left(pwd) => pwd
      case Right((chars, len)) => PasswordUtils.generate(len, chars)
    }
    val db2 = Database.updatePassword(db1, key, pwd)
    path(db2.id).bytes = Database.serialize(passphrase, db2)
    head = db2.id
  }

  def current(passphrase: SecureString): Database = Database.deserialize(passphrase, path(head).bytes)

  def head: UUID = UUID.fromString(new File(directory, "HEAD").text.trim)
  def head_=(id: UUID): Unit = new File(directory, "HEAD").text = id.toString
  def path(id: UUID): File = new File(directory, id.toString)

  def run(args: Array[String]): Unit = {
    val cla = new CommandLineArguments(args)
    cla.subcommand match {
      case Some(cla.init) =>
        val pp = cli.readSecureString("Passphrase") match {
          case Some(pp) =>
            cli.readSecureString("Repeat") match {
              case Some(ppRepeat) if pp == ppRepeat =>
                ppRepeat.wipe()
                pp
              case Some(ppRepeat) =>
                ppRepeat.wipe()
                pp.wipe()
                throw new Exception("Passphrase repetition does not match")
              case None =>
                throw new Exception("Passphrase repetition does not match")
            }
          case _ => throw new Exception("You must provide a passphrase")
        }

        try {
          init(pp)
        } finally {
          pp.wipe()
        }

        cli.printSuccess("Created new password store")
      case Some(cla.list) =>
        for (pwd <- passphrase(cli)(list(_)).sortWith(_.key < _.key)) {
          cli.printInfo(pwd.key, s"${pwd.name} (${PasswordUtils.getBitEntropy(pwd.password)} bits) ${pwd.timeStamp}")
        }
      case Some(cla.history) =>
        val db = passphrase(cli)(current(_))
        for (i <- 0 until db.versions.length - 1) {
          val v1 = db.versions(i + 1)
          val v2 = db.versions(i)
          val diff = Database.diff(db, v1, v2)

          cli.println(v2.timeStamp)

          for (a <- diff) a match {
            case (key, (None, Some(b))) => cli.printInfo(key, s"Add password (${b.id})")
            case (key, (Some(a), None)) => cli.printInfo(key, s"Remove password (${a.id})")
            case (key, (Some(a), Some(b))) => cli.printInfo(key, s"Update password (${a.id} -> ${b.id})")
            case (_, (None, None)) => throw new Exception("Impossible case")
          }
        }
      case Some(cla.show) =>
        passphrase(cli)(show(_, cla.show.idOrKey())) match {
          case Some(pwd) =>
            cli.printInfo(pwd.key, "Password information")
            cli.println(s"  Timestamp: ${pwd.timeStamp}")
            cli.println(s"  Name: ${pwd.name}")
            if (pwd.description.length > 0) cli.println(s"  Description: ${pwd.description}")
            if (pwd.userName.length > 0) cli.println(s"  Username: ${pwd.userName}")
            if (pwd.url.length > 0) cli.println(s"  URL: ${pwd.url}")
            cli.println(s"  Strength: ${PasswordUtils.getBitEntropy(pwd.password)} bits")
            if (cla.show.printPassword()) {
              cli.print("  Password: ")
              pwd.password.read(_.foreach(cli.print(_)))
              cli.println("")
            } else {
              cli.println("  Password: ***")
            }
          case _ =>
            cli.printError(s"Password ${cla.show.idOrKey()} does not exist")
        }
      case Some(cla.get) =>
        passphrase(cli)(show(_, cla.show.idOrKey())) match {
          case Some(pwd) =>
            pwd.password.read(plain => Clipboard.put(plain.mkString))
            cli.printSuccess(s"Copied password to clipboard")
          case _ =>
            cli.printError(s"Password ${cla.show.idOrKey()} does not exist")
        }
      case Some(cla.add) =>
        val name = cli.readWithDefault("Name", cla.add.key())
        val description = cli.read("Description").orElse(Some("")).get
        val url = cli.read("URL").orElse(Some("")).get
        val userName = cli.read("Username").orElse(Some("")).get
        val pwd = cli.readSecureString("Password") match {
          case Some(pwd) =>
            cli.readSecureString("Repeat") match {
              case Some(pwdRepeat) if pwd == pwdRepeat => Left(pwd)
              case _ => throw new Exception("Password repetition does not match")
            }
          case _ =>
            val length = cli.readWithDefault[Int]("Password length", 32, _.toInt)
            val alphaLower = cli.readWithDefault[Boolean]("Use lower alpha characters?", true, _.toBoolean)
            val alphaUpper = cli.readWithDefault[Boolean]("Use upper alpha characters?", true, _.toBoolean)
            val numbers = cli.readWithDefault[Boolean]("Use number characters?", true, _.toBoolean)
            val special = cli.readWithDefault[Boolean]("Use special characters?", true, _.toBoolean)
            Right((PasswordCharacters(alphaLower, alphaUpper, numbers, special), length))
        }
        passphrase(cli)(add(_, cla.add.key(), pwd, name, description, userName, url, Map.empty))
        cli.printSuccess("Added password")
      case Some(cla.remove) =>
        passphrase(cli)(remove(_, cla.remove.idOrKey()))
        cli.printSuccess("Removed password")
      case Some(cla.renew) =>
        val pwd = cli.readSecureString("Password") match {
          case Some(pwd) =>
            cli.readSecureString("Repeat") match {
              case Some(pwdRepeat) if pwd == pwdRepeat => Left(pwd)
              case _ => throw new Exception("Password repetition does not match")
            }
          case _ =>
            val length = cli.readWithDefault[Int]("Password length", 32, _.toInt)
            val alphaLower = cli.readWithDefault[Boolean]("Use lower alpha characters?", true, _.toBoolean)
            val alphaUpper = cli.readWithDefault[Boolean]("Use upper alpha characters?", true, _.toBoolean)
            val numbers = cli.readWithDefault[Boolean]("Use number characters?", true, _.toBoolean)
            val special = cli.readWithDefault[Boolean]("Use special characters?", true, _.toBoolean)
            Right((PasswordCharacters(alphaLower, alphaUpper, numbers, special), length))
        }
        passphrase(cli)(renew(_, cla.renew.key(), pwd))
        cli.printSuccess("Updated password")
      case Some(cla.sync) =>
        config match {
          case Config(Some(syncConnInfo), Some(syncRemoteDir)) =>
            passphrase(cli)(Sync.synchronize(_, directory, config.syncConnInfo.get, config.syncRemoteDir.get))
          case _ => throw new Exception("You have not properly configured the remote to sync with")
        }
        cli.printSuccess("Synchronized password store with remote")
      case _ =>
        // TODO print help on cli object
        cla.printHelp()
    }
  }

  private def passphrase[T](cli: CommandLineInterface)(inner: SecureString => T): T = cli.readSecureString("Passphrase") match {
    case Some(pp) =>
      try {
        inner(pp)
      } finally {
        pp.wipe()
      }
    case _ => throw new Exception("You must provide a passphrase")
  }

  private def parseIdOrKey(idOrKey: String): Either[UUID, String] = {
    try {
      Left(UUID.fromString(idOrKey))
    } catch {
      case e: IllegalArgumentException => Right(idOrKey)
    }
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
