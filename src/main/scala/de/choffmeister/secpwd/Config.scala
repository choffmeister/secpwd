package de.choffmeister.secpwd

import java.io.File
import java.util.Properties
import de.choffmeister.secpwd.utils.SshConnectionInfo
import de.choffmeister.secpwd.utils.RichFile._
import de.choffmeister.secpwd.utils.SshConnectionInfo
import java.io.StringReader
import java.io.FileNotFoundException

case class Config(syncConnInfo: Option[SshConnectionInfo] = None, syncRemoteDir: Option[String] = None)

object Config {
  def load(dir: File): Config = {
    try {
      val prop = new Properties()
      val confFile = new File(dir, "config")
      prop.load(new StringReader(confFile.text))

      val host = Option(prop.getProperty("host"))
      val userName = Option(prop.getProperty("username"))
      val syncConnInfo = if (!host.isDefined || !userName.isDefined) None
      else Some(SshConnectionInfo(
        host = host.get,
        userName = userName.get,
        password = Option(prop.getProperty("password")),
        keyFile = Option(prop.getProperty("keyfile")) match {
          case Some(kf) => Some(new File(kf))
          case _ => None
        },
        keyFilePass = Option(prop.getProperty("keyfile_pass")) match {
          case Some(kfp) => Some(kfp.getBytes("UTF-8"))
          case _ => None
        },
        port = Option(prop.getProperty("port")) match {
          case Some(p) => p.toInt
          case _ => 22
        }
      ))

      Config(
        syncConnInfo = syncConnInfo,
        syncRemoteDir = Option(prop.getProperty("remote_dir"))
      )
    } catch {
      case e: FileNotFoundException => Config()
    }
  }
}