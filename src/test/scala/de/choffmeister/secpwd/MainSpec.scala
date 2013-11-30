package de.choffmeister.secpwd

import org.specs2.mutable._
import org.specs2.runner.JUnitRunner
import org.junit.runner.RunWith
import java.io.File
import java.io.EOFException
import java.util.UUID
import de.choffmeister.secpwd.security.PasswordCharacters
import de.choffmeister.secpwd.utils.RichFile._
import de.choffmeister.secpwd.utils.NullCommandLineInterface
import de.choffmeister.securestring.SecureString
import de.choffmeister.secpwd.utils.NullCommandLineInterface
import de.choffmeister.secpwd.utils.MockCommandLineInterface
import de.choffmeister.secpwd.utils.SftpClientSpec

@RunWith(classOf[JUnitRunner])
class MainSpec extends Specification {
  def tmp = new File(System.getProperty("java.io.tmpdir"), UUID.randomUUID.toString)

  "init, add, remove, show and allow HEAD change" in {
    val pp = SecureString("password".toCharArray)
    val main = new Main(tmp, new NullCommandLineInterface())
    main.init(pp)
    main.list(pp) === Nil
    val state1 = main.head

    val pwd1 = main.add(pp, "gmail", Left(SecureString("pass".toCharArray)), "Google Mail", "", "user", "http://www.googlemail.com", Map.empty)
    pwd1.password.read(_.mkString === "pass")
    main.list(pp) === List(pwd1)
    val state2 = main.head

    val pwd2 = main.add(pp, "amazon", Right((PasswordCharacters(true, true, true, true), 16)), "Amazon", "", "user2", "amazon.com", Map.empty)
    pwd2.password.read(_.mkString must haveSize(16))
    main.list(pp) === List(pwd2, pwd1)
    val state3 = main.head

    main.show(pp, "gmail") === pwd1
    main.show(pp, pwd2.id.toString) === pwd2

    main.remove(pp, "gmail")
    main.list(pp) === List(pwd2)
    val state4 = main.head

    main.remove(pp, pwd2.id.toString)
    main.list(pp) === Nil
    val state5 = main.head

    main.setHead(state1)
    main.list(pp) === Nil
    main.setHead(state2)
    main.list(pp) === List(pwd1)
    main.setHead(state3)
    main.list(pp) === List(pwd2, pwd1)
    main.setHead(state4)
    main.list(pp) === List(pwd2)
    main.setHead(state5)
    main.list(pp) === Nil
  }

  "detect manipulations of database" in {
    val pp = SecureString("password".toCharArray)
    val main = new Main(tmp, new NullCommandLineInterface())
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

  "secpwd init - succeed" in {
    val dir = tmp
    val cli = new MockCommandLineInterface()
    val main = new Main(dir, cli)
    cli.queueInput(Some("pass"))
    cli.queueInput(Some("pass"))
    main.run(Array("init"))
    ok
  }

  "secpwd init - fail due to no passphrase" in {
    val dir = tmp
    val cli = new MockCommandLineInterface()
    val main = new Main(dir, cli)
    cli.queueInput(None)
    main.run(Array("init")) must throwA
  }

  "secpwd init - fail due to no repitition" in {
    val dir = tmp
    val cli = new MockCommandLineInterface()
    val main = new Main(dir, cli)
    cli.queueInput(Some("pass"))
    cli.queueInput(None)
    main.run(Array("init")) must throwA
  }

  "secpwd init - fail due to repitition mismatch" in {
    val dir = tmp
    val cli = new MockCommandLineInterface()
    val main = new Main(dir, cli)
    cli.queueInput(Some("pass"))
    cli.queueInput(Some("pass2"))
    main.run(Array("init")) must throwA
  }

  "secpwd init - fail due to double initialization" in {
    val dir = tmp
    val cli = new MockCommandLineInterface()
    val main = new Main(dir, cli)
    cli.queueInput(Some("pass"))
    cli.queueInput(Some("pass"))
    main.run(Array("init"))

    cli.queueInput(Some("pass2"))
    cli.queueInput(Some("pass2"))
    main.run(Array("init")) must throwA
  }

  "secpwd add - succeed with custom password" in {
    val dir = tmp
    val cli = new MockCommandLineInterface()
    val main = new Main(dir, cli)
    cli.queueInput(Some("pp"))
    cli.queueInput(Some("pp"))
    main.run(Array("init"))

    cli.queueInput(Some("pp"))
    cli.queueInput(Some("Google Mail"))
    cli.queueInput(None)
    cli.queueInput(Some("https://googlemail.com/"))
    cli.queueInput(Some("invalid.user@googlemail.com"))
    cli.queueInput(Some("pass"))
    cli.queueInput(Some("pass"))
    main.run(Array("add", "gmail"))
    ok
  }

  "secpwd add - succeed with generated password" in {
    val dir = tmp
    val cli = new MockCommandLineInterface()
    val main = new Main(dir, cli)
    cli.queueInput(Some("pp"))
    cli.queueInput(Some("pp"))
    main.run(Array("init"))

    cli.queueInput(Some("pp"))
    cli.queueInput(Some("Google Mail"))
    cli.queueInput(None)
    cli.queueInput(Some("https://googlemail.com/"))
    cli.queueInput(Some("invalid.user@googlemail.com"))
    cli.queueInput(None)
    cli.queueInput(Some("16"))
    cli.queueInput(None)
    cli.queueInput(None)
    cli.queueInput(None)
    cli.queueInput(Some("false"))
    main.run(Array("add", "gmail"))
    ok
  }

  "secpwd list - succeed" in {
    val dir = tmp
    val cli = new MockCommandLineInterface()
    val main = new Main(dir, cli)
    cli.queueInput(Some("pp"))
    cli.queueInput(Some("pp"))
    main.run(Array("init"))

    cli.queueInput(Some("pp"))
    cli.queueInput(Some("Google Mail"))
    cli.queueInput(None)
    cli.queueInput(Some("https://googlemail.com/"))
    cli.queueInput(Some("invalid.user@googlemail.com"))
    cli.queueInput(None)
    cli.queueInput(Some("16"))
    cli.queueInput(None)
    cli.queueInput(None)
    cli.queueInput(None)
    cli.queueInput(Some("false"))
    main.run(Array("add", "gmail"))

    cli.queueInput(Some("pp"))
    main.run(Array("list"))
    ok
  }

  "secpwd history - succeed" in {
    val dir = tmp
    val cli = new MockCommandLineInterface()
    val main = new Main(dir, cli)
    cli.queueInput(Some("pp"))
    cli.queueInput(Some("pp"))
    main.run(Array("init"))

    cli.queueInput(Some("pp"))
    cli.queueInput(Some("Google Mail"))
    cli.queueInput(None)
    cli.queueInput(Some("https://googlemail.com/"))
    cli.queueInput(Some("invalid.user@googlemail.com"))
    cli.queueInput(None)
    cli.queueInput(Some("16"))
    cli.queueInput(None)
    cli.queueInput(None)
    cli.queueInput(None)
    cli.queueInput(Some("false"))
    main.run(Array("add", "gmail"))

    cli.queueInput(Some("pp"))
    main.run(Array("history"))
    ok
  }

  "secpwd show - succeed" in {
    val dir = tmp
    val cli = new MockCommandLineInterface()
    val main = new Main(dir, cli)
    cli.queueInput(Some("pp"))
    cli.queueInput(Some("pp"))
    main.run(Array("init"))

    cli.queueInput(Some("pp"))
    cli.queueInput(Some("Google Mail"))
    cli.queueInput(None)
    cli.queueInput(Some("https://googlemail.com/"))
    cli.queueInput(Some("invalid.user@googlemail.com"))
    cli.queueInput(None)
    cli.queueInput(Some("16"))
    cli.queueInput(None)
    cli.queueInput(None)
    cli.queueInput(None)
    cli.queueInput(Some("false"))
    main.run(Array("add", "gmail"))

    cli.queueInput(Some("pp"))
    main.run(Array("show", "gmail"))
    ok
  }

  "secpwd remove - succeed" in {
    val dir = tmp
    val cli = new MockCommandLineInterface()
    val main = new Main(dir, cli)
    cli.queueInput(Some("pp"))
    cli.queueInput(Some("pp"))
    main.run(Array("init"))

    cli.queueInput(Some("pp"))
    cli.queueInput(Some("Google Mail"))
    cli.queueInput(None)
    cli.queueInput(Some("https://googlemail.com/"))
    cli.queueInput(Some("invalid.user@googlemail.com"))
    cli.queueInput(None)
    cli.queueInput(Some("16"))
    cli.queueInput(None)
    cli.queueInput(None)
    cli.queueInput(None)
    cli.queueInput(Some("false"))
    main.run(Array("add", "gmail"))

    cli.queueInput(Some("pp"))
    main.run(Array("remove", "gmail"))
    ok
  }

  "secpwd renew - succeed" in {
    val dir = tmp
    val cli = new MockCommandLineInterface()
    val main = new Main(dir, cli)
    cli.queueInput(Some("pp"))
    cli.queueInput(Some("pp"))
    main.run(Array("init"))

    cli.queueInput(Some("pp"))
    cli.queueInput(Some("Google Mail"))
    cli.queueInput(None)
    cli.queueInput(Some("https://googlemail.com/"))
    cli.queueInput(Some("invalid.user@googlemail.com"))
    cli.queueInput(None)
    cli.queueInput(Some("16"))
    cli.queueInput(None)
    cli.queueInput(None)
    cli.queueInput(None)
    cli.queueInput(Some("false"))
    main.run(Array("add", "gmail"))

    cli.queueInput(Some("pp"))
    cli.queueInput(Some("pass2"))
    cli.queueInput(Some("pass2"))
    main.run(Array("renew", "gmail"))
    ok
  }

  "secpwd sync - succeed" in {
    val dir = tmp
    val cli = new MockCommandLineInterface()
    val conf = Config(
      syncConnInfo = Some(SftpClientSpec.testConnInfo),
      syncRemoteDir = Some("/tmp/" + UUID.randomUUID().toString)
    )
    val main = new Main(dir, cli, conf)
    cli.queueInput(Some("pp"))
    cli.queueInput(Some("pp"))
    main.run(Array("init"))

    cli.queueInput(Some("pp"))
    cli.queueInput(Some("Google Mail"))
    cli.queueInput(None)
    cli.queueInput(Some("https://googlemail.com/"))
    cli.queueInput(Some("invalid.user@googlemail.com"))
    cli.queueInput(None)
    cli.queueInput(Some("16"))
    cli.queueInput(None)
    cli.queueInput(None)
    cli.queueInput(None)
    cli.queueInput(Some("false"))
    main.run(Array("add", "gmail"))

    cli.queueInput(Some("pp"))
    main.run(Array("remove", "gmail"))
    
    cli.queueInput(Some("pp"))
    main.run(Array("sync"))

    new File(main.directory, "HEAD").text = ""

    cli.queueInput(Some("pp"))
    main.run(Array("sync"))
    ok
  }

  "usage" in {
    val dir = tmp
    val cli = new MockCommandLineInterface()
    val conf = Config(
      syncConnInfo = Some(SftpClientSpec.testConnInfo),
      syncRemoteDir = Some("/tmp/" + UUID.randomUUID().toString)
    )
    val main = new Main(dir, cli, conf)

    cli.queueInput(Some("pp"))
    cli.queueInput(Some("pp"))
    cli.out.write("$ secpwd init\n")
    main.run(Array("init"))

    cli.queueInput(Some("pp"))
    cli.out.write("$ secpwd list\n")
    main.run(Array("list"))

    cli.queueInput(Some("pp"))
    cli.queueInput(Some("Google Mail"))
    cli.queueInput(None)
    cli.queueInput(Some("https://googlemail.com/"))
    cli.queueInput(Some("invalid.user@googlemail.com"))
    cli.queueInput(Some("my-gmail-pass"))
    cli.queueInput(Some("my-gmail-pass"))
    cli.out.write("$ secpwd add gmail\n")
    main.run(Array("add", "gmail"))

    cli.queueInput(Some("pp"))
    cli.out.write("$ secpwd list\n")
    main.run(Array("list"))

    cli.queueInput(Some("pp"))
    cli.out.write("$ secpwd show gmail\n")
    main.run(Array("show", "gmail"))

    cli.queueInput(Some("pp"))
    cli.queueInput(None)
    cli.queueInput(Some("24"))
    cli.queueInput(None)
    cli.queueInput(None)
    cli.queueInput(None)
    cli.queueInput(Some("false"))
    cli.out.write("$ secpwd renew gmail\n")
    main.run(Array("renew", "gmail"))

    cli.queueInput(Some("pp"))
    cli.out.write("$ secpwd show gmail\n")
    main.run(Array("show", "gmail"))

    cli.queueInput(Some("pp"))
    cli.out.write("$ secpwd show -p gmail\n")
    main.run(Array("show", "-p", "gmail"))

    println(cli)

    ok
  }

  def dropByte(bytes: Array[Byte], index: Int) = bytes.take(index) ++ bytes.drop(index + 1)
  def alterByte(bytes: Array[Byte], index: Int) = bytes.take(index) ++ Array[Byte](if (bytes(index) >= 0) -1 else 1) ++ bytes.drop(index + 1)
  def insertByte(bytes: Array[Byte], index: Int) = bytes.take(index) ++ Array[Byte](0) ++ bytes.drop(index)
}