package de.choffmeister.secpwd

import java.io._
import com.jcraft.jsch._
import scala.language.reflectiveCalls

case class SshConnectionInfo(host: String = "", userName: String = "", password: String = "", port: Int = 22)

object SftpClient {
  def write(connInfo: SshConnectionInfo, targetDir: String, targetName: String, input: InputStream) {
    sftp(connInfo) { sftp =>
      sftp.cd(targetDir)
      sftp.put(input, targetName)
    }
  }

  def read(connInfo: SshConnectionInfo, sourceDir: String, sourceName: String, output: OutputStream) {
    sftp(connInfo) { sftp =>
      sftp.cd(sourceDir)
      pipe(sftp.get(sourceName), output)
    }
  }

  private def sftp(connInfo: SshConnectionInfo)(inner: ChannelSftp => Any) {
    val jsch = new JSch()

    using(jsch.getSession(connInfo.userName, connInfo.host, connInfo.port)) { session =>
      session.setPassword(connInfo.password)
      session.setConfig("StrictHostKeyChecking", "no")
      session.connect()

      using(session.openChannel("sftp").asInstanceOf[ChannelSftp]) { sftp =>
        sftp.connect()
        inner(sftp)
      }
    }
  }

  private def pipe(source: InputStream, target: OutputStream) {
    val buffer = new Array[Byte](1024)
    val bis = new BufferedInputStream(source)
    val bos = new BufferedOutputStream(target)
    var done = false

    while (!done) {
      val read = bis.read(buffer, 0, buffer.length)

      if (read > 0) bos.write(buffer, 0, read)
      else done = true
    }

    bis.close()
    bos.close()
  }

  private def using[T <: { def disconnect(): Unit }](disconnectable: T)(inner: T => Any) {
    try {
      inner(disconnectable)
    } finally {
      disconnectable.disconnect()
    }
  }
}
