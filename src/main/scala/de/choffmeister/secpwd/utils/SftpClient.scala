package de.choffmeister.secpwd.utils

import java.io._
import com.jcraft.jsch._
import scala.language.reflectiveCalls

case class SshConnectionInfo(host: String = "", userName: String = "", password: Option[String] = None, keyFile: Option[File] = None, keyFilePass: Option[Array[Byte]] = None, port: Int = 22)
class SshConnectionException(msg: String, inner: Throwable) extends Exception(msg, inner)
class SshAuthenticationException(inner: Throwable) extends SshConnectionException("Authentication failed", inner)

class SftpSession(channel: ChannelSftp) {
  def write(targetDir: String, targetName: String, input: InputStream) {
    channel.cd(targetDir)
    channel.put(input, targetName)
  }

  def read(sourceDir: String, sourceName: String, output: OutputStream) {
    channel.cd(sourceDir)
    pipe(channel.get(sourceName), output)
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
}

object SftpClient {
  def connect[T](connInfo: SshConnectionInfo)(inner: SftpSession => T): T = {
    sftp(connInfo) { channel =>
      val session = new SftpSession(channel)
      inner(session)
    }
  }

  private def sftp[T](connInfo: SshConnectionInfo)(inner: ChannelSftp => T): T = {
    val jsch = new JSch()
    connInfo match {
      case SshConnectionInfo(_, _, _, Some(keyFile), Some(keyFilePass), _) => 
        jsch.addIdentity(keyFile.getAbsolutePath, keyFilePass)
      case SshConnectionInfo(_, _, _, Some(keyFile), None, _) => 
        jsch.addIdentity(keyFile.getAbsolutePath)
      case _ =>
    }

    using(jsch.getSession(connInfo.userName, connInfo.host, connInfo.port)) { session =>
      if (connInfo.password.isDefined) session.setPassword(connInfo.password.get)
      session.setConfig("StrictHostKeyChecking", "no")

      try {
        session.connect()
      } catch {
        case ex: JSchException if ex.getMessage == "Auth cancel" => throw new SshAuthenticationException(ex)
        case ex: JSchException if ex.getMessage == "USERAUTH fail" => throw new SshAuthenticationException(ex)
        case _: Throwable => throw new Exception("ASD")
      }

      using(session.openChannel("sftp").asInstanceOf[ChannelSftp]) { sftp =>
        sftp.connect()
        inner(sftp)
      }
    }
  }

  private def using[A <: { def disconnect(): Unit }, B](disconnectable: A)(inner: A => B): B = {
    try {
      inner(disconnectable)
    } finally {
      disconnectable.disconnect()
    }
  }
}
