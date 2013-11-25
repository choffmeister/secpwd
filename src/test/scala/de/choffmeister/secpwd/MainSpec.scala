package de.choffmeister.secpwd

import org.specs2.mutable._
import org.specs2.runner.JUnitRunner
import org.junit.runner.RunWith
import java.io.File
import java.io.EOFException
import java.util.UUID
import de.choffmeister.secpwd.security.PasswordCharacters
import de.choffmeister.secpwd.utils.RichFile._
import de.choffmeister.securestring.SecureString

@RunWith(classOf[JUnitRunner])
class MainSpec extends Specification {
  def tmp = new File(System.getProperty("java.io.tmpdir"), UUID.randomUUID.toString)

  "init, add, remove, show and allow HEAD change" in {
    val pp = SecureString("password".toCharArray)
    val main = new Main(tmp)
    main.init(pp)
    main.list(pp) === Nil
    val state1 = main.head

    val pwd1 = main.add(pp, "gmail", "googlemail.com", Left(SecureString("pass".toCharArray)))
    pwd1.password.read(_.mkString === "pass")
    main.list(pp) === List(pwd1)
    val state2 = main.head

    val pwd2 = main.add(pp, "amazon", "amazon.com", Right((PasswordCharacters(true, true, true, true), 16)))
    pwd2.password.read(_.mkString must haveSize(16))
    main.list(pp) === List(pwd2, pwd1)
    val state3 = main.head

    main.show(pp, "gmail") === Some(pwd1)
    main.show(pp, pwd2.id.toString) === Some(pwd2)

    main.remove(pp, "gmail")
    main.list(pp) === List(pwd2)
    val state4 = main.head

    main.remove(pp, pwd2.id.toString)
    main.list(pp) === Nil
    val state5 = main.head

    main.head = state1
    main.list(pp) === Nil
    main.head = state2
    main.list(pp) === List(pwd1)
    main.head = state3
    main.list(pp) === List(pwd2, pwd1)
    main.head = state4
    main.list(pp) === List(pwd2)
    main.head = state5
    main.list(pp) === Nil
  }

  "detect manipulations of database" in {
    val pp = SecureString("password".toCharArray)
    val main = new Main(tmp)
    val db = main.init(pp)
    main.list(pp) === Nil

    val path = main.path(db.id)
    val original = path.bytes
    path.bytes = alterByte(original, 0)
    main.list(pp) must throwA[DatabaseSerializationException]

    path.bytes = alterByte(original, 4)
    main.list(pp) must throwA[DatabaseSerializationException]

    path.bytes = alterByte(original, 20)
    main.list(pp) must throwA

    path.bytes = dropByte(original, original.length - 1)
    main.list(pp) must throwA

    path.bytes = alterByte(original, original.length - 1)
    main.list(pp) must throwA

    path.bytes = original ++ Array[Byte](1)
    main.list(pp) must throwA

    ok
  }
  
  def dropByte(bytes: Array[Byte], index: Int) = bytes.take(index) ++ bytes.drop(index + 1)
  def alterByte(bytes: Array[Byte], index: Int) = bytes.take(index) ++ Array[Byte](if (bytes(index) >= 0) -1 else 1) ++ bytes.drop(index + 1)
  def insertByte(bytes: Array[Byte], index: Int) = bytes.take(index) ++ Array[Byte](0) ++ bytes.drop(index)
}