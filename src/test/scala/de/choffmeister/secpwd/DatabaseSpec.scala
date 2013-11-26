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
    val pwd1 = DatabaseSpec.createPasswordEntry(0)
    val db2 = Database.addPassword(db1, pwd1)
    val pwd2 = DatabaseSpec.createPasswordEntry(1)
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
    val pwd1 = DatabaseSpec.createPasswordEntry(0)
    val db2 = Database.addPassword(db1, pwd1)
    val pwd2 = DatabaseSpec.createPasswordEntry(1)
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
    val pwd1 = DatabaseSpec.createPasswordEntry(0)
    val db2 = Database.addPassword(db1, pwd1)
    val pwd2 = DatabaseSpec.createPasswordEntry(1)
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

  "deserialize and serialize password entries" in {
    val pwd1 = DatabaseSpec.createPasswordEntry(0).copy(customFields = Map("field1" -> "value1", "field2" -> "value2"))
    val ba1 = new ByteArrayOutputStream()
    Database.serializePasswordEnty(ba1, pwd1)
    val ba2 = new ByteArrayInputStream(ba1.toByteArray)
    val pwd2 = Database.deserializePasswordEntry(ba2)

    pwd2.id === pwd1.id
    pwd2.timeStamp === new Date(0)
    pwd2.key === "pwd-0"
    pwd2.password === SecureString("pass-0".toCharArray())
    pwd2.name === "Password 0"
    pwd2.description === "Description 0"
    pwd2.userName === "user-0"
    pwd2.url === "http://service0.com/"
    pwd2.customFields === Map("field1" -> "value1", "field2" -> "value2")
  }

  "deserialize and serialize databases" in {
    val db1 = Database.create()
    val pwd1 = DatabaseSpec.createPasswordEntry(0)
    val db2 = Database.addPassword(db1, pwd1)
    val pwd2 = DatabaseSpec.createPasswordEntry(1)
    val db3 = Database.addPassword(db2, pwd2)

    val output = new ByteArrayOutputStream()
    Database.serializeDatabase(output, db3)
    val input = new ByteArrayInputStream(output.toByteArray)
    val db4 = Database.deserializeDatabase(input)

    db4 === db3
  }

  "diff" in {
    val db1 = Database.create()
    val pwd1 = DatabaseSpec.createPasswordEntry(0)
    val db2 = Database.addPassword(db1, pwd1)
    val pwd2 = DatabaseSpec.createPasswordEntry(1)
    val db3 = Database.addPassword(db2, pwd2)
    val pwd3 = DatabaseSpec.createPasswordEntry(2)
    val db4 = Database.addPassword(db3, pwd3)
    val pwd4 = pwd1.copy(id = UUID.randomUUID(), timeStamp = new Date(3), password = SecureString("password1-new".toCharArray))
    val db5 = Database.updatePassword(db4, pwd4)
    val db6 = Database.removePasswordByKey(db5, "pwd-1")
    
    val db = db6
    val v = (0 until 6).map(i => db.versions(5 - i))

    Database.diff(db, v(0), v(1)) === Map("pwd-0" -> (None, Some(pwd1)))
    Database.diff(db, v(1), v(2)) === Map("pwd-1" -> (None, Some(pwd2)))
    Database.diff(db, v(2), v(3)) === Map("pwd-2" -> (None, Some(pwd3)))
    Database.diff(db, v(3), v(4)) === Map("pwd-0" -> (Some(pwd1), Some(pwd4)))
    Database.diff(db, v(4), v(5)) === Map("pwd-1" -> (Some(pwd2), None))

    Database.diff(db, v(0), v(3)) === Map("pwd-2" -> (None, Some(pwd3)), "pwd-1" -> (None, Some(pwd2)), "pwd-0" -> (None, Some(pwd1)))
    Database.diff(db, v(0), v(5)) === Map("pwd-0" -> (None, Some(pwd4)), "pwd-2" -> (None, Some(pwd3)))
    Database.diff(db, v(3), v(5)) === Map("pwd-1" -> (Some(pwd2), None), "pwd-0" -> (Some(pwd1), Some(pwd4)))
  }

  "find lowest common ancestor" in {
    val db1 = Database.create()
    val pwd1 = DatabaseSpec.createPasswordEntry(0)
    val db2 = Database.addPassword(db1, pwd1)
    val pwd2 = DatabaseSpec.createPasswordEntry(1)
    val db3 = Database.addPassword(db2, pwd2)

    val pwd3a = DatabaseSpec.createPasswordEntry(2)
    val db4a = Database.addPassword(db3, pwd3a)
    val pwd4a = DatabaseSpec.createPasswordEntry(3)
    val db5a = Database.addPassword(db4a, pwd4a)
    val pwd5a = DatabaseSpec.createPasswordEntry(4)
    val db6a = Database.addPassword(db5a, pwd5a)

    val pwd3b = DatabaseSpec.createPasswordEntry(5)
    val db4b = Database.addPassword(db3, pwd3b)
    val pwd4b = DatabaseSpec.createPasswordEntry(6)
    val db5b = Database.addPassword(db4b, pwd4b)
    val pwd5b = DatabaseSpec.createPasswordEntry(7)
    val db6b = Database.addPassword(db5b, pwd5b)

    Database.lowestCommonAncestor(db3, db3.versions(0), db3, db3.versions(0)) === db3.versions(0)
    Database.lowestCommonAncestor(db1, db1.versions(0), db3, db3.versions(0)) === db1.versions(0)
    Database.lowestCommonAncestor(db3, db3.versions(0), db1, db1.versions(0)) === db1.versions(0)
    Database.lowestCommonAncestor(db4a, db4a.versions(0), db4b, db4b.versions(0)) === db3.versions(0)
    Database.lowestCommonAncestor(db5a, db5a.versions(0), db5b, db5b.versions(0)) === db3.versions(0)
    Database.lowestCommonAncestor(db6a, db6a.versions(0), db6b, db5b.versions(0)) === db3.versions(0)
    Database.lowestCommonAncestor(db4a, db4a.versions(0), db6b, db5b.versions(0)) === db3.versions(0)
  }
  
  "merge" in {
    val db1 = Database.create()
    val pwd1 = DatabaseSpec.createPasswordEntry(0)
    val db2 = Database.addPassword(db1, pwd1)
    val pwd2 = DatabaseSpec.createPasswordEntry(1)
    val db3 = Database.addPassword(db2, pwd2)

    val pwd3a = DatabaseSpec.createPasswordEntry(2)
    val db4a = Database.addPassword(db3, pwd3a)
    val pwd4a = DatabaseSpec.createPasswordEntry(3)
    val db5a = Database.addPassword(db4a, pwd4a)
    val db6a = Database.removePasswordByKey(db5a, "pwd-2")

    val pwd3b = DatabaseSpec.createPasswordEntry(4)
    val db4b = Database.addPassword(db3, pwd3b)
    val pwd4b = DatabaseSpec.createPasswordEntry(5)
    val db5b = Database.addPassword(db4b, pwd4b)
    val db6b = Database.removePasswordByKey(db5b, "pwd-4")

    Database.merge(db3, db3.versions(0), db3, db3.versions(0)) === db3
    Database.merge(db1, db1.versions(0), db3, db3.versions(0)) === db3
    Database.merge(db3, db3.versions(0), db1, db1.versions(0)) === db3

    val merged1 = Database.merge(db4a, db4a.versions(0), db4b, db4b.versions(0))
    merged1 === Database(
      merged1.id,
      merged1.timeStamp,
      DatabaseVersion(
        merged1.id,
        merged1.timeStamp,
        4,
        List(db4a.id, db4b.id),
        (pwd3b :: pwd3a :: db3.passwords).map(_.id)) :: db4a.versions(0) :: db4b.versions(0) :: db3.versions,
      pwd3a :: pwd3b :: db3.passwords
    )

    val merged2 = Database.merge(db6a, db6a.versions(0), db6b, db6b.versions(0))
    merged2 === Database(
      merged2.id,
      merged2.timeStamp,
      DatabaseVersion(
        merged2.id,
        merged2.timeStamp,
        6,
        List(db6a.id, db6b.id),
        (pwd4b :: pwd4a :: db3.passwords).map(_.id)) :: db6a.versions(0) :: db6b.versions(0) :: db5a.versions(0) :: db5b.versions(0) :: db4a.versions(0) :: db4b.versions(0) :: db3.versions,
      pwd4a :: pwd3a :: pwd4b :: pwd3b :: db3.passwords
    )

    val otherDb = Database.create()
    Database.merge(db6b, db6b.versions(0), otherDb, otherDb.versions(0)) must throwA
  }

  "from version" in {
    val db1 = Database.create()
    val pwd1 = DatabaseSpec.createPasswordEntry(0)
    val db2 = Database.addPassword(db1, pwd1)
    val pwd2 = DatabaseSpec.createPasswordEntry(1)
    val db3 = Database.addPassword(db2, pwd2)
    val pwd3a = DatabaseSpec.createPasswordEntry(2)
    val db4a = Database.addPassword(db3, pwd3a)
    val pwd4a = DatabaseSpec.createPasswordEntry(3)
    val db5a = Database.addPassword(db4a, pwd4a)
    val pwd5a = DatabaseSpec.createPasswordEntry(4)
    val db6a = Database.addPassword(db5a, pwd5a)
    val pwd3b = DatabaseSpec.createPasswordEntry(5)
    val db4b = Database.addPassword(db3, pwd3b)
    val pwd4b = DatabaseSpec.createPasswordEntry(6)
    val db5b = Database.addPassword(db4b, pwd4b)
    val pwd5b = DatabaseSpec.createPasswordEntry(7)
    val db6b = Database.addPassword(db5b, pwd5b)

    Database.fromVersion(db6b, db6b.versions(0)) === db6b
    Database.fromVersion(db6b, db6b.versions(1)) === db5b
    Database.fromVersion(db6b, db6b.versions(2)) === db4b
    Database.fromVersion(db6b, db6b.versions(3)) === db3
    Database.fromVersion(db6b, db6b.versions(4)) === db2
    Database.fromVersion(db6b, db6b.versions(5)) === db1
  }
}

object DatabaseSpec {
  def createPasswordEntry(i: Int) = PasswordEntry(UUID.randomUUID(), new Date(i), s"pwd-$i", SecureString(s"pass-$i".toCharArray), s"Password $i", s"Description $i", s"user-$i", s"http://service$i.com/", Map.empty)
}