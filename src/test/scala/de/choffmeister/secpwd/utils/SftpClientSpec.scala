package de.choffmeister.secpwd.utils

import org.specs2.mutable._
import org.specs2.runner.JUnitRunner
import org.junit.runner.RunWith
import java.util.UUID
import java.io.{File, InputStream, ByteArrayInputStream, ByteArrayOutputStream}

@RunWith(classOf[JUnitRunner])
class SftpClientSpec extends Specification {
  "read and write files (unencrypted keyfile)" in {
    val name = UUID.randomUUID.toString
    val text1 = UUID.randomUUID.toString
    SftpClient.connect(SftpClientSpec.testConnInfo) { session =>
      streamIn(text1)(session.write("/tmp", name, _) must throwA.not.orSkip)
      val text2 = streamOut(session.read("/tmp", name, _))
      text1 === text2
    } must throwA[SshAuthenticationException].not.orSkip
  }

  "read and write files (encrypted keyfile)" in {
    val name = UUID.randomUUID.toString
    val text1 = UUID.randomUUID.toString
    SftpClient.connect(SftpClientSpec.testConnInfo2) { session =>
      streamIn(text1)(session.write("/tmp", name, _) must throwA.not.orSkip)
      val text2 = streamOut(session.read("/tmp", name, _))
      text1 === text2
    } must throwA[SshAuthenticationException].not.orSkip
  }

  "fail on invalid credentials" in {
    val connInfo = SshConnectionInfo(host = "localhost", userName = "unknownuser", password = Some("wrong-password"))
    SftpClient.connect(connInfo)(session => 0 === 1) must throwA[SshAuthenticationException]
  }

  def streamIn(content: String)(inner: InputStream => Any): Unit = {
    val bytes = content.getBytes("UTF-8")
    val stream = new ByteArrayInputStream(bytes)
    inner(stream)
  }

  def streamOut(inner: ByteArrayOutputStream => Any): String = {
    val stream = new ByteArrayOutputStream()
    inner(stream)
    new String(stream.toByteArray, "UTF-8")
  }
}

object SftpClientSpec {
  val testConnInfo = SshConnectionInfo(
    host = "localhost",
    userName = System.getProperty("user.name"),
    keyFile = Some(new File(new File(new File(System.getProperty("user.home")), ".ssh"), "id-test"))
  )

  val testConnInfo2 = SshConnectionInfo(
    host = "localhost",
    userName = System.getProperty("user.name"),
    keyFile = Some(new File(new File(new File(System.getProperty("user.home")), ".ssh"), "id-test2")),
    keyFilePass = Some("password".getBytes("UTF-8"))
  )
}
