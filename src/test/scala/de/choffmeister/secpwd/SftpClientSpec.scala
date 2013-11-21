package de.choffmeister.secpwd

import org.specs2.mutable._
import org.specs2.runner.JUnitRunner
import org.junit.runner.RunWith

import java.util.UUID
import java.io.{InputStream, ByteArrayInputStream, ByteArrayOutputStream}

@RunWith(classOf[JUnitRunner])
class SftpClientSpec extends Specification {
  args(skipAll = true)

  "read and write files" in {
    val connInfo = SshConnectionInfo(host = "invalid.domain.tld", userName = "user", password = "pass")

    val name = UUID.randomUUID.toString
    val text1 = UUID.randomUUID.toString
    streamIn(text1)(SftpClient.write(connInfo, "/tmp", name, _))
    val text2 = streamOut(SftpClient.read(connInfo, "/tmp", name, _))

    text1 === text2
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
