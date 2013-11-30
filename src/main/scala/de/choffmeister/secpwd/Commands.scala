package de.choffmeister.secpwd

import java.util.UUID
import de.choffmeister.secpwd.utils.RichFile._
import de.choffmeister.secpwd.utils.Clipboard
import de.choffmeister.secpwd.security.PasswordUtils
import de.choffmeister.securestring.SecureString
import de.choffmeister.secpwd.security.PasswordCharacters
import java.util.Date

trait Command {
  def parseIdOrKey(idOrKey: String): Either[UUID, String] = {
    try {
      Left(UUID.fromString(idOrKey))
    } catch {
      case e: IllegalArgumentException => Right(idOrKey)
    }
  }
}

object InitCommand extends Command {
  def apply(main: Main): Unit = {
    val pp = main.cli.readSecureStringWithRepitition("Passphrase") match {
      case Some(pp) =>
        try {
          headless(main, pp)
          main.cli.printSuccess("Created new password store")
        } finally {
          pp.wipe()
        }
      case _ => throw new Exception("You must provide a passphrase")
    }
  }
  
  def headless(main: Main, passphrase: SecureString): Database = {
    if (main.hasCurrent) throw new Exception("Database alreay exists")

    val db = Database.create()
    main.setCurrent(db, passphrase)
    main.cli.printSuccess("Created new password store")
    db
  }
}

object ListCommand extends Command {
  def apply(main: Main, passphrase: SecureString): Unit = {
    headless(main, passphrase)
  }

  def headless(main: Main, passphrase: SecureString): List[PasswordEntry] = {
    val db = main.current(passphrase)
    val passwords = db.currentPasswords.sortWith(_.key < _.key)
    for (pwd <- passwords) {
      main.cli.printInfo(pwd.key, s"${pwd.name} (${PasswordUtils.getBitEntropy(pwd.password)} bits) ${pwd.timeStamp}")
    }
    passwords
  }
}

object HistoryCommand extends Command {
  def apply(main: Main, passphrase: SecureString): Unit = {
    val changes = headless(main, passphrase)

    for (change <- changes) {
      main.cli.println(change._2.timeStamp)

      for (diff <- change._3) diff match {
        case (key, (None, Some(b))) => main.cli.printInfo(key, s"Add password (${b.id})")
        case (key, (Some(a), None)) => main.cli.printInfo(key, s"Remove password (${a.id})")
        case (key, (Some(a), Some(b))) => main.cli.printInfo(key, s"Update password (${a.id} -> ${b.id})")
        case (_, (None, None)) => throw new Exception("Impossible case")
      }
    }
  }

  def headless(main: Main, passphrase: SecureString): List[(DatabaseVersion, DatabaseVersion, Map[String, (Option[PasswordEntry], Option[PasswordEntry])])] = {
    val db = main.current(passphrase)

    (0 until db.versions.length - 1)
      .map(i => (db.versions(i + 1), db.versions(i)))
      .map(vs => (vs._1, vs._2, Database.diff(db, vs._1, vs._2)))
      .toList
  }
}

object ShowCommand extends Command {
  def apply(main: Main, idOrKey: String, printPassword: Boolean, passphrase: SecureString): Unit = {
    val pwd = headless(main, idOrKey, passphrase)

    main.cli.printInfo(pwd.key, "Password information")
    main.cli.println(s"  Timestamp: ${pwd.timeStamp}")
    main.cli.println(s"  Name: ${pwd.name}")
    if (pwd.description.length > 0) main.cli.println(s"  Description: ${pwd.description}")
    if (pwd.userName.length > 0) main.cli.println(s"  Username: ${pwd.userName}")
    if (pwd.url.length > 0) main.cli.println(s"  URL: ${pwd.url}")
    main.cli.println(s"  Strength: ${PasswordUtils.getBitEntropy(pwd.password)} bits")
    if (printPassword) {
      main.cli.print("  Password: ")
      pwd.password.read(_.foreach(main.cli.print(_)))
      main.cli.println("")
    } else {
      main.cli.println("  Password: ***")
    }
  }

  def headless(main: Main, idOrKey: String, passphrase: SecureString): PasswordEntry = {
    val db = main.current(passphrase)
    val pwd = parseIdOrKey(idOrKey) match {
      case Left(id) => db.passwordById(id)
      case Right(key) => db.currentPasswordByKey(key)
    }
    pwd match {
      case Some(pwd) => pwd
      case _ => throw new Exception(s"Password ${idOrKey} does not exist")
    }
  }
}

object GetCommand extends Command {
  def apply(main: Main, idOrKey: String, passphrase: SecureString): Unit = {
    val pwd = headless(main, idOrKey, passphrase)

    pwd.password.read(plain => Clipboard.put(plain.mkString))
    main.cli.printSuccess(s"Copied password to clipboard")
  }

  def headless(main: Main, idOrKey: String, passphrase: SecureString): PasswordEntry = {
    val db = main.current(passphrase)
    val pwd = parseIdOrKey(idOrKey) match {
      case Left(id) => db.passwordById(id)
      case Right(key) => db.currentPasswordByKey(key)
    }
    pwd match {
      case Some(pwd) => pwd
      case _ => throw new Exception(s"Password ${idOrKey} does not exist")
    }
  }
}

object AddCommand extends Command {
  def apply(main: Main, key: String, passphrase: SecureString): Unit = {
    val name = main.cli.readWithDefault("Name", key)
    val description = main.cli.read("Description").orElse(Some("")).get
    val url = main.cli.read("URL").orElse(Some("")).get
    val userName = main.cli.read("Username").orElse(Some("")).get
    val pwd = main.cli.readSecureStringWithRepitition("Password") match {
      case Some(pwd) => Left(pwd)
      case _ =>
        val length = main.cli.readWithDefault[Int]("Password length", 32, _.toInt)
        val alphaLower = main.cli.readWithDefault[Boolean]("Use lower alpha characters?", true, _.toBoolean)
        val alphaUpper = main.cli.readWithDefault[Boolean]("Use upper alpha characters?", true, _.toBoolean)
        val numbers = main.cli.readWithDefault[Boolean]("Use number characters?", true, _.toBoolean)
        val special = main.cli.readWithDefault[Boolean]("Use special characters?", true, _.toBoolean)
        Right((PasswordCharacters(alphaLower, alphaUpper, numbers, special), length))
    }

    headless(main, key, pwd, name, description, userName, url, Map.empty, passphrase)
    main.cli.printSuccess("Added password")
  }

  def headless(main: Main, key: String, password: Either[SecureString, (PasswordCharacters, Int)], name: String, description: String, userName: String, url: String, customFields: Map[String, String], passphrase: SecureString): PasswordEntry = {
    val db1 = main.current(passphrase)
    val pwd = password match {
      case Left(pwd) => pwd
      case Right((chars, len)) => PasswordUtils.generate(len, chars)
    }
    val entry = PasswordEntry(UUID.randomUUID(), new Date(), key, pwd, name, description, userName, url, customFields)
    val db2 = Database.addPassword(db1, entry)
    main.setCurrent(db2, passphrase)
    entry
  }
}

object RemoveCommand extends Command {
  def apply(main: Main, idOrKey: String, passphrase: SecureString): Unit = {
    headless(main, idOrKey, passphrase)
    main.cli.printSuccess("Removed password")
  }

  def headless(main: Main, idOrKey: String, passphrase: SecureString): Unit = {
    val db1 = main.current(passphrase)
    val db2 = parseIdOrKey(idOrKey) match {
      case Left(id) => Database.removePasswordById(db1, id)
      case Right(key) => Database.removePasswordByKey(db1, key)
    }
    main.setCurrent(db2, passphrase)
  }
}

object RenewCommand {
  def apply(main: Main, key: String, passphrase: SecureString): Unit = {
    val pwd = main.cli.readSecureStringWithRepitition("Password") match {
      case Some(pwd) => Left(pwd)
      case _ =>
        val length = main.cli.readWithDefault[Int]("Password length", 32, _.toInt)
        val alphaLower = main.cli.readWithDefault[Boolean]("Use lower alpha characters?", true, _.toBoolean)
        val alphaUpper = main.cli.readWithDefault[Boolean]("Use upper alpha characters?", true, _.toBoolean)
        val numbers = main.cli.readWithDefault[Boolean]("Use number characters?", true, _.toBoolean)
        val special = main.cli.readWithDefault[Boolean]("Use special characters?", true, _.toBoolean)
        Right((PasswordCharacters(alphaLower, alphaUpper, numbers, special), length))
    }

    headless(main, key, pwd, passphrase)
    main.cli.printSuccess("Updated password")
  }

  def headless(main: Main, key: String, password: Either[SecureString, (PasswordCharacters, Int)], passphrase: SecureString): Unit = {
    val db1 = main.current(passphrase)
    val pwd = password match {
      case Left(pwd) => pwd
      case Right((chars, len)) => PasswordUtils.generate(len, chars)
    }
    val db2 = Database.updatePassword(db1, key, pwd)
    main.setCurrent(db2, passphrase)
  }
}

object SyncCommand extends Command {
  def apply(main: Main, passphrase: SecureString): Unit = {
    headless(main, passphrase) match {
      case (Some(local), Some(remote), merged) =>
        if (local == merged && remote == merged) main.cli.printSuccess("Already up-to-date")
        else {
          val diff1 = Database.diff(merged, local.versions(0), merged.versions(0))
          val diff2 = Database.diff(merged, remote.versions(0), merged.versions(0))

          for (change <- diff1) {
            change match {
              case (key, (None, Some(_))) => main.cli.printInfo(key, "Added locally")
              case (key, (Some(_), None)) => main.cli.printInfo(key, "Removed locally")
              case (key, (Some(_), Some(_))) => main.cli.printInfo(key, "Modified locally")
              case (key, (None, None)) => throw new Exception("Impossible case")
            }
          }
          for (change <- diff2) {
            change match {
              case (key, (None, Some(_))) => main.cli.printInfo(key, "Added at remote")
              case (key, (Some(_), None)) => main.cli.printInfo(key, "Removed at remote")
              case (key, (Some(_), Some(_))) => main.cli.printInfo(key, "Modified at remote")
              case (key, (None, None)) => throw new Exception("Impossible case")
            }
          }
          main.cli.printSuccess("Synchronized")
        }
      case (Some(local), None, _) =>
        main.cli.printSuccess("Copied local to remote")
      case (None, Some(remote), _) =>
        main.cli.printSuccess("Copied remote to local")
      case (None, None, _) => throw new Exception("Impossible case")
    }
  }

  def headless(main: Main, passphrase: SecureString): (Option[Database], Option[Database], Database) = {
    main.config match {
      case Config(Some(syncConnInfo), Some(syncRemoteDir)) =>
        Sync.synchronize(main, passphrase, main.config.syncConnInfo.get, main.config.syncRemoteDir.get)
      case _ => throw new Exception("You have not properly configured the remote to sync with")
    }
  }
}