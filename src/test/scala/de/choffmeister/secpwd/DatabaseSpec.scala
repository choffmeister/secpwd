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

    db.versions === List(DatabaseVersion(db.id, db.timeStamp, 0, Nil, Nil))
    db.passwords === Nil
  }

  "add passwords" in {
    val db1 = Database.create()
    val pwd1 = PasswordEntry(UUID.randomUUID(), new Date(0), "service1", "Service #1", SecureString("password1".toCharArray))
    val db2 = Database.addPassword(db1, pwd1)
    val pwd2 = PasswordEntry(UUID.randomUUID(), new Date(1), "service2", "Service #2", SecureString("password2".toCharArray))
    val db3 = Database.addPassword(db2, pwd2)

    val v1 = DatabaseVersion(db1.id, db1.timeStamp, 0, Nil, Nil)
    val v2 = DatabaseVersion(db2.id, db2.timeStamp, 1, List(db1.id), List(pwd1.id))
    val v3 = DatabaseVersion(db3.id, db3.timeStamp, 2, List(db2.id), List(pwd2.id, pwd1.id))

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

    val v1 = DatabaseVersion(db1.id, db1.timeStamp, 0, Nil, Nil)
    val v2 = DatabaseVersion(db2.id, db2.timeStamp, 1, List(db1.id), List(pwd1.id))
    val v3 = DatabaseVersion(db3.id, db3.timeStamp, 2, List(db2.id), List(pwd2.id, pwd1.id))
    val v4 = DatabaseVersion(db4.id, db4.timeStamp, 3, List(db3.id), List(pwd2.id))
    val v5 = DatabaseVersion(db5.id, db5.timeStamp, 4, List(db4.id), Nil)

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
    val pwd3 = pwd1.copy(id = UUID.randomUUID(), timeStamp = new Date(2), password = SecureString("password1-new".toCharArray))
    val db4 = Database.updatePassword(db3, pwd3)
    val db5 = Database.updatePassword(db4, pwd2.key, SecureString("password2-new".toCharArray))

    val pwd4 = db5.passwords(0)
    val v1 = DatabaseVersion(db1.id, db1.timeStamp, 0, Nil, Nil)
    val v2 = DatabaseVersion(db2.id, db2.timeStamp, 1, List(db1.id), List(pwd1.id))
    val v3 = DatabaseVersion(db3.id, db3.timeStamp, 2, List(db2.id), List(pwd2.id, pwd1.id))
    val v4 = DatabaseVersion(db4.id, db4.timeStamp, 3, List(db3.id), List(pwd3.id, pwd2.id))
    val v5 = DatabaseVersion(db5.id, db5.timeStamp, 4, List(db4.id), List(pwd4.id, pwd3.id))

    pwd4.password.read(_.mkString === "password2-new")
    db5.versions === List(v5, v4, v3, v2, v1)
    db5.passwords === List(pwd4, pwd3, pwd2, pwd1)
    db4.versions === List(v4, v3, v2, v1)
    db4.passwords === List(pwd3, pwd2, pwd1)
    db3.versions === List(v3, v2, v1)
    db3.passwords === List(pwd2, pwd1)
    db2.versions === List(v2, v1)
    db2.passwords === List(pwd1)
    db1.versions === List(v1)
    db1.passwords === List()
    db5.passwords.map(_.id).distinct must haveSize(4)
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

  "diff" in {
    val db1 = Database.create()
    val pwd1 = PasswordEntry(UUID.randomUUID(), new Date(0), "service1", "Service #1", SecureString("password1".toCharArray))
    val db2 = Database.addPassword(db1, pwd1)
    val pwd2 = PasswordEntry(UUID.randomUUID(), new Date(1), "service2", "Service #2", SecureString("password2".toCharArray))
    val db3 = Database.addPassword(db2, pwd2)
    val pwd3 = PasswordEntry(UUID.randomUUID(), new Date(2), "service3", "Service #3", SecureString("password3".toCharArray))
    val db4 = Database.addPassword(db3, pwd3)
    val pwd4 = pwd1.copy(id = UUID.randomUUID(), timeStamp = new Date(3), password = SecureString("password1-new".toCharArray))
    val db5 = Database.updatePassword(db4, pwd4)
    val db6 = Database.removePasswordByKey(db5, "service2")
    
    val db = db6
    val v = (0 until 6).map(i => db.versions(5 - i))

    Database.diff(db, v(0), v(1)) === Map("service1" -> (None, Some(pwd1)))
    Database.diff(db, v(1), v(2)) === Map("service2" -> (None, Some(pwd2)), "service1" -> (Some(pwd1), Some(pwd1)))
    Database.diff(db, v(2), v(3)) === Map("service3" -> (None, Some(pwd3)), "service2" -> (Some(pwd2), Some(pwd2)), "service1" -> (Some(pwd1), Some(pwd1)))
    Database.diff(db, v(3), v(4)) === Map("service3" -> (Some(pwd3), Some(pwd3)), "service2" -> (Some(pwd2), Some(pwd2)), "service1" -> (Some(pwd1), Some(pwd4)))
    Database.diff(db, v(4), v(5)) === Map("service2" -> (Some(pwd2), None), "service1" -> (Some(pwd4), Some(pwd4)), "service3" -> (Some(pwd3), Some(pwd3)))

    Database.diff(db, v(0), v(5)) === Map("service1" -> (None, Some(pwd4)), "service3" -> (None, Some(pwd3)))
    Database.diff(db, v(3), v(5)) === Map("service2" -> (Some(pwd2), None), "service3" -> (Some(pwd3), Some(pwd3)), "service1" -> (Some(pwd1), Some(pwd4)))
  }

  "find lowest common ancestor" in {
    val db1 = Database.create()
    val pwd1 = PasswordEntry(UUID.randomUUID(), new Date(0), "service1", "Service #1", SecureString("password1".toCharArray))
    val db2 = Database.addPassword(db1, pwd1)
    val pwd2 = PasswordEntry(UUID.randomUUID(), new Date(1), "service2", "Service #2", SecureString("password2".toCharArray))
    val db3 = Database.addPassword(db2, pwd2)

    val pwd3a = PasswordEntry(UUID.randomUUID(), new Date(2), "service3a", "Service #3a", SecureString("password3a".toCharArray))
    val db4a = Database.addPassword(db3, pwd3a)
    val pwd4a = PasswordEntry(UUID.randomUUID(), new Date(2), "service4a", "Service #4a", SecureString("password4a".toCharArray))
    val db5a = Database.addPassword(db4a, pwd4a)
    val pwd5a = PasswordEntry(UUID.randomUUID(), new Date(2), "service5a", "Service #5a", SecureString("password5a".toCharArray))
    val db6a = Database.addPassword(db5a, pwd5a)

    val pwd3b = PasswordEntry(UUID.randomUUID(), new Date(2), "service3b", "Service #3b", SecureString("password3b".toCharArray))
    val db4b = Database.addPassword(db3, pwd3b)
    val pwd4b = PasswordEntry(UUID.randomUUID(), new Date(2), "service4b", "Service #4b", SecureString("password4b".toCharArray))
    val db5b = Database.addPassword(db4b, pwd4b)
    val pwd5b = PasswordEntry(UUID.randomUUID(), new Date(2), "service5b", "Service #5b", SecureString("password5b".toCharArray))
    val db6b = Database.addPassword(db5b, pwd5b)

    Database.lowestCommonAncestor(db3, db3.versions(0), db3, db3.versions(0)) === db3.versions(0)
    Database.lowestCommonAncestor(db1, db1.versions(0), db3, db3.versions(0)) === db1.versions(0)
    Database.lowestCommonAncestor(db3, db3.versions(0), db1, db1.versions(0)) === db1.versions(0)
    Database.lowestCommonAncestor(db4a, db4a.versions(0), db4b, db4b.versions(0)) === db3.versions(0)
    Database.lowestCommonAncestor(db5a, db5a.versions(0), db5b, db5b.versions(0)) === db3.versions(0)
    Database.lowestCommonAncestor(db6a, db6a.versions(0), db6b, db5b.versions(0)) === db3.versions(0)
    Database.lowestCommonAncestor(db4a, db4a.versions(0), db6b, db5b.versions(0)) === db3.versions(0)
  }
}