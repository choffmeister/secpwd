package de.choffmeister.secpwd

import org.specs2.mutable._
import org.specs2.runner.JUnitRunner
import org.junit.runner.RunWith
import java.util.{Date, UUID}
import java.io.{ByteArrayInputStream, ByteArrayOutputStream}
import de.choffmeister.securestring.SecureString

@RunWith(classOf[JUnitRunner])
class DatabaseSpec extends Specification {
  "start empty" in {
    val db = Database.create()

    db.versions === List(DatabaseVersion(db.id, db.timeStamp, Nil, Nil))
    db.passwords === Nil
  }

  "add passwords" in {
    val db1 = Database.create()
    val pwd1 = PasswordEntry(UUID.randomUUID(), new Date(0), "service1", "Service #1", SecureString("password1".toCharArray))
    val db2 = Database.addPassword(db1, pwd1)
    val pwd2 = PasswordEntry(UUID.randomUUID(), new Date(1), "service2", "Service #2", SecureString("password2".toCharArray))
    val db3 = Database.addPassword(db2, pwd2)

    val v1 = DatabaseVersion(db1.id, db1.timeStamp, Nil, Nil)
    val v2 = DatabaseVersion(db2.id, db2.timeStamp, List(db1.id), List(pwd1.id))
    val v3 = DatabaseVersion(db3.id, db3.timeStamp, List(db2.id), List(pwd2.id, pwd1.id))

    db1.versions === List(v1)
    db1.passwords === List()
    db2.versions === List(v2, v1)
    db2.passwords === List(pwd1)
    db3.versions === List(v3, v2, v1)
    db3.passwords === List(pwd2, pwd1)
  }

  "remove passwords" in {
    val db1 = Database.create()
    val pwd1 = PasswordEntry(UUID.randomUUID(), new Date(0), "service1", "Service #1", SecureString("password1".toCharArray))
    val db2 = Database.addPassword(db1, pwd1)
    val pwd2 = PasswordEntry(UUID.randomUUID(), new Date(1), "service2", "Service #2", SecureString("password2".toCharArray))
    val db3 = Database.addPassword(db2, pwd2)
    val db4 = Database.removePasswordByKey(db3, pwd1.key)
    val db5 = Database.removePasswordByKey(db4, pwd2.key)

    val v1 = DatabaseVersion(db1.id, db1.timeStamp, Nil, Nil)
    val v2 = DatabaseVersion(db2.id, db2.timeStamp, List(db1.id), List(pwd1.id))
    val v3 = DatabaseVersion(db3.id, db3.timeStamp, List(db2.id), List(pwd2.id, pwd1.id))
    val v4 = DatabaseVersion(db4.id, db4.timeStamp, List(db3.id), List(pwd2.id))
    val v5 = DatabaseVersion(db5.id, db5.timeStamp, List(db4.id), Nil)

    db3.versions === List(v3, v2, v1)
    db3.passwords === List(pwd2, pwd1)
    db4.versions === List(v4, v3, v2, v1)
    db4.passwords === List(pwd2, pwd1)
    db5.versions === List(v5, v4, v3, v2, v1)
    db5.passwords === List(pwd2, pwd1)
  }

  "update passwords" in {
    val db1 = Database.create()
    val pwd1 = PasswordEntry(UUID.randomUUID(), new Date(0), "service1", "Service #1", SecureString("password1".toCharArray))
    val db2 = Database.addPassword(db1, pwd1)
    val pwd2 = PasswordEntry(UUID.randomUUID(), new Date(1), "service2", "Service #2", SecureString("password2".toCharArray))
    val db3 = Database.addPassword(db2, pwd2)
    val pwd3 = pwd1.copy(timeStamp = new Date(2), password = SecureString("password1-new".toCharArray))
    val db4 = Database.updatePassword(db3, pwd3)
    val db5 = Database.updatePassword(db4, pwd2.key, SecureString("password2-new".toCharArray))

    val pwd4 = db5.passwords(0)
    val v1 = DatabaseVersion(db1.id, db1.timeStamp, Nil, Nil)
    val v2 = DatabaseVersion(db2.id, db2.timeStamp, List(db1.id), List(pwd1.id))
    val v3 = DatabaseVersion(db3.id, db3.timeStamp, List(db2.id), List(pwd2.id, pwd1.id))
    val v4 = DatabaseVersion(db4.id, db4.timeStamp, List(db3.id), List(pwd3.id, pwd2.id))
    val v5 = DatabaseVersion(db5.id, db5.timeStamp, List(db4.id), List(pwd4.id, pwd3.id))

    pwd4.password.read(_.mkString === "password2-new")
    db5.versions === List(v5, v4, v3, v2, v1)
    db5.passwords === List(pwd4, pwd3, pwd2, pwd1)
    db4.versions === List(v4, v3, v2, v1)
    db4.passwords === List(pwd3, pwd2, pwd1)
    db3.versions === List(v3, v2, v1)
    db3.passwords === List(pwd2, pwd1)
  }

  "deseralize and serialize databases" in {
    val db1 = Database.create()
    val pwd1 = PasswordEntry(UUID.randomUUID(), new Date(0), "service1", "Service #1", SecureString("password1".toCharArray))
    val db2 = Database.addPassword(db1, pwd1)
    val pwd2 = PasswordEntry(UUID.randomUUID(), new Date(1), "service2", "Service #2", SecureString("password2".toCharArray))
    val db3 = Database.addPassword(db2, pwd2)

    val output = new ByteArrayOutputStream()
    Database.serializeDatabase(output, db3)
    val input = new ByteArrayInputStream(output.toByteArray)
    val db4 = Database.deserializeDatabase(input)

    db4 === db3
  }
}