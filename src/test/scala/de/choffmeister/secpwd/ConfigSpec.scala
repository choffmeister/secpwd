package de.choffmeister.secpwd

import org.specs2.mutable._
import org.specs2.runner.JUnitRunner
import org.junit.runner.RunWith
import java.io.File
import java.util.UUID
import de.choffmeister.secpwd.utils.RichFile._
import de.choffmeister.secpwd.utils.SshConnectionInfo

@RunWith(classOf[JUnitRunner])
class ConfigSpec extends Specification {
  def tmp = new File(System.getProperty("java.io.tmpdir"), UUID.randomUUID.toString)

  "load not existing" in {
    val dir = tmp
    dir.mkdirs()

    val confFile = new File(dir, "config")
    Config.load(dir) === Config()
  }

  "load empty" in {
    val dir = tmp
    dir.mkdirs()

    val confFile = new File(dir, "config")
    confFile.text = ""
    Config.load(dir) === Config()
  }

  "load non empty" in {
    val dir = tmp
    dir.mkdirs()
    val confFile = new File(dir, "config")

    confFile.text = """
host = test-host
username = test-username"""

    Config.load(dir) === Config(
        Some(SshConnectionInfo(
            host = "test-host",
            userName = "test-username"
        )),
        None
    )

    confFile.text = """
host = test-host
username = test-username
password = test-password
keyfile = /tmp/keyfile
remote_dir = /tmp/remote_dir
port = 22222"""

    Config.load(dir) === Config(
        Some(SshConnectionInfo(
            host = "test-host",
            userName = "test-username",
            password = Some("test-password"),
            keyFile = Some(new File("/tmp/keyfile")),
            keyFilePass = None,
            port = 22222
        )),
        Some("/tmp/remote_dir")
    )

    confFile.text = """
host = test-host
username = test-username
keyfile_pass = test-keyfile-pass"""

    Config.load(dir).syncConnInfo.get.keyFilePass.get.toList === "test-keyfile-pass".getBytes("UTF-8").toList
  }
}
