package de.choffmeister.secpwd

import org.specs2.mutable._
import org.specs2.runner.JUnitRunner
import org.junit.runner.RunWith
import java.util.Date

@RunWith(classOf[JUnitRunner])
class DatabaseSpec extends Specification {
  "start empty" in {
    val db = Database.create()

    db.parentIds must beEmpty
    db.passwords must beEmpty
  }

  "add passwords" in {
    val db1 = Database.create()
    val pwd1 = PasswordEntry("service1", new Date(0), "Service #1", "password1")
    val db2 = Database.addPassword(db1, pwd1)

    db2.parentIds === List(db1.id)
    db2.passwords === List(pwd1)
    db1.passwords === List()

    val pwd2 = PasswordEntry("service2", new Date(1), "Service #2", "password2")
    val db3 = Database.addPassword(db2, pwd2)

    db3.parentIds === List(db2.id)
    db3.passwords === List(pwd2, pwd1)
    db2.passwords === List(pwd1)
  }

  "remove passwords" in {
    val db1 = Database.create()
    val pwd1 = PasswordEntry("service1", new Date(0), "Service #1", "password1")
    val db2 = Database.addPassword(db1, pwd1)
    val pwd2 = PasswordEntry("service2", new Date(1), "Service #2", "password2")
    val db3 = Database.addPassword(db2, pwd2)

    val db4 = Database.removePasswordById(db3, pwd1.id)
    db4.parentIds === List(db3.id)
    db4.passwords === List(pwd2)
    db3.passwords === List(pwd2, pwd1)

    val db5 = Database.removePasswordById(db4, pwd2.id)
    db5.parentIds === List(db4.id)
    db5.passwords === List()
    db4.passwords === List(pwd2)
  }

  "update passwords" in {
    val db1 = Database.create()
    val pwd1 = PasswordEntry("service1", new Date(0), "Service #1", "password1")
    val db2 = Database.addPassword(db1, pwd1)
    val pwd2 = PasswordEntry("service2", new Date(1), "Service #2", "password2")
    val db3 = Database.addPassword(db2, pwd2)

    val pwd3 = pwd1.copy(password = "password1-new")
    val db4 = Database.updatePassword(db3, pwd3)
    db4.parentIds === List(db3.id)
    db4.passwords === List(pwd3, pwd2)
    db3.passwords === List(pwd2, pwd1)

    val db5 = Database.updatePassword(db4, pwd2.id, "password2-new")
    db5.parentIds === List(db4.id)
    db5.passwords(1) === pwd3
    db5.passwords(0).id == pwd2.id
    db5.passwords(0).password === "password2-new"
    db4.passwords === List(pwd3, pwd2)
  }
}