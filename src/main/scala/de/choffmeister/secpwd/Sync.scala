package de.choffmeister.secpwd

import java.io._
import java.util._
import de.choffmeister.secpwd.utils.RichFile._
import de.choffmeister.secpwd.utils.SshConnectionInfo
import de.choffmeister.secpwd.utils.SftpClient
import de.choffmeister.securestring.SecureString

object Sync {
  def synchronize(main: Main, passphrase: SecureString, remoteConnInfo: SshConnectionInfo, remoteDir: String): (Option[Database], Option[Database], Database) = {
    SftpClient.connect(remoteConnInfo) { session =>
      if (!session.exists(remoteDir, ".")) session.mkdir(remoteDir)
      
      val localExists = main.hasCurrent
      val remoteExists = session.exists(remoteDir, "HEAD")

      (localExists, remoteExists) match {
        case (true, true) =>
          val remoteHead = UUID.fromString(streamStringOut(session.read(remoteDir, "HEAD", _)))
          val remoteDb = Database.deserialize(passphrase, streamBytesOut(session.read(remoteDir, remoteHead.toString, _)))
          val localHead = main.head
          val localDb = main.current(passphrase)
          val mergedDb = Database.merge(localDb, localDb.versions(0), remoteDb, remoteDb.versions(0))
          val mergedDbBytes = Database.serialize(passphrase, mergedDb)

          if (localDb != mergedDb) {
            main.setCurrent(mergedDb, passphrase)
          }

          if (remoteDb != mergedDb) {
            streamBytesIn(mergedDbBytes)(session.write(remoteDir, mergedDb.id.toString, _))
            streamStringIn(mergedDb.id.toString)(session.write(remoteDir, "HEAD", _))
          }

          (Some(localDb), Some(remoteDb), mergedDb)
        case (true, false) =>
          val localHead = main.head
          val localDbBytes = main.currentRaw
          val localDb = main.current(passphrase)
          streamBytesIn(localDbBytes)(session.write(remoteDir, localDb.id.toString, _))
          streamStringIn(localDb.id.toString)(session.write(remoteDir, "HEAD", _))
          (Some(localDb), None, localDb)
        case (false, true) =>
          val remoteHead = UUID.fromString(streamStringOut(session.read(remoteDir, "HEAD", _)))
          val remoteDbBytes = streamBytesOut(session.read(remoteDir, remoteHead.toString, _))
          val remoteDb = Database.deserialize(passphrase, remoteDbBytes)
          main.setCurrent(remoteDb, passphrase)
          (None, Some(remoteDb), remoteDb)
        case (false, false) =>
          throw new Exception("Neither a local nor a remote database exists")
      }
    }
  }

  private def streamStringIn(str: String)(inner: InputStream => Any): Unit = streamBytesIn(str.getBytes("UTF-8"))(inner)
  private def streamStringOut(inner: ByteArrayOutputStream => Any): String = new String(streamBytesOut(inner), "UTF-8")
  private def streamBytesIn(bytes: Array[Byte])(inner: InputStream => Any): Unit = {
    val stream = new ByteArrayInputStream(bytes)
    inner(stream)
  }
  private def streamBytesOut(inner: ByteArrayOutputStream => Any): Array[Byte] = {
    val stream = new ByteArrayOutputStream()
    inner(stream)
    stream.toByteArray
  }
}