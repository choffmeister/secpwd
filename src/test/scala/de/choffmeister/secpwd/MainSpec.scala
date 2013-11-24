package de.choffmeister.secpwd

import org.specs2.mutable._
import org.specs2.runner.JUnitRunner
import org.junit.runner.RunWith
import java.io.File
import java.io.EOFException
import java.util.UUID
import de.choffmeister.secpwd.security.PasswordCharacters
import de.choffmeister.secpwd.utils.RichFile._

@RunWith(classOf[JUnitRunner])
class MainSpec extends Specification {
  def tmp = new File(System.getProperty("java.io.tmpdir"), UUID.randomUUID.toString)

  "init, add, remove and allow HEAD change" in {
    val main = new Main(tmp, "password".toCharArray)
    main.init
    main.list === Nil
    val state1 = main.head

    val pwd1 = main.add("gmail", "googlemail.com", Left("pass".toCharArray))
    pwd1.password.mkString === "pass"
    main.list === List(pwd1)
    val state2 = main.head

    val pwd2 = main.add("amazon", "amazon.com", Right((PasswordCharacters(true, true, true, true), 16)))
    pwd2.password.mkString must haveSize(16)
    main.list === List(pwd2, pwd1)
    val state3 = main.head

    main.remove("gmail")
    main.list === List(pwd2)
    val state4 = main.head

    main.head = state1
    main.list === Nil
    main.head = state2
    main.list === List(pwd1)
    main.head = state3
    main.list === List(pwd2, pwd1)
    main.head = state4
    main.list === List(pwd2)
  }

  "detect manipulations of database" in {
    val main = new Main(tmp, "password2".toCharArray)
    val db = main.init
    main.list === Nil

    val path = main.path(db.id)
    val original = path.bytes
    path.bytes = alterByte(original, 0)
    main.list must throwA[DatabaseSerializationException]

    path.bytes = alterByte(original, 5)
    main.list must throwA[EOFException]

    path.bytes = alterByte(original, 10)
    main.list must throwA

    path.bytes = alterByte(original, 20)
    main.list must throwA[DatabaseSerializationException]

    path.bytes = dropByte(original, original.length - 1)
    main.list must throwA[EOFException]

    path.bytes = alterByte(original, original.length - 1)
    main.list must throwA[DatabaseSerializationException]

    path.bytes = original ++ Array[Byte](1)
    main.list must throwA[DatabaseSerializationException]

    ok
  }
  
  def dropByte(bytes: Array[Byte], index: Int) = bytes.take(index) ++ bytes.drop(index + 1)
  def alterByte(bytes: Array[Byte], index: Int) = bytes.take(index) ++ Array[Byte](if (bytes(index) >= 0) -1 else 1) ++ bytes.drop(index + 1)
  def insertByte(bytes: Array[Byte], index: Int) = bytes.take(index) ++ Array[Byte](0) ++ bytes.drop(index)
}