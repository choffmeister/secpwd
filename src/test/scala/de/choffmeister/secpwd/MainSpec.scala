package de.choffmeister.secpwd

import org.specs2.mutable._
import org.specs2.runner.JUnitRunner
import org.junit.runner.RunWith
import java.io.File
import java.util.UUID
import de.choffmeister.secpwd.security.PasswordCharacters

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
}