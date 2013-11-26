package de.choffmeister.secpwd

import java.io._
import java.util._
import de.choffmeister.secpwd.utils.RichFile._
import de.choffmeister.secpwd.utils.SshConnectionInfo
import de.choffmeister.secpwd.utils.SftpClient
import de.choffmeister.securestring.SecureString

object Sync {
  def synchronize(passphrase: SecureString, localDir: File, remoteConnInfo: SshConnectionInfo, remoteDir: String): Database = {
    SftpClient.connect(remoteConnInfo) { session =>
      if (!session.exists(remoteDir, ".")) session.mkdir(remoteDir)

      if (session.exists(remoteDir, "HEAD")) {
        val remoteHead = UUID.fromString(streamStringOut(session.read(remoteDir, "HEAD", _)))
        val remoteDb = Database.deserialize(passphrase, streamBytesOut(session.read(remoteDir, remoteHead.toString, _)))
        val localHead = head(localDir)
        val localDb = Database.deserialize(passphrase, path(localDir, localHead).bytes)
        val mergedDb = Database.merge(localDb, localDb.versions(0), remoteDb, remoteDb.versions(0))
        val mergedDbBytes = Database.serialize(passphrase, mergedDb)

        if (localDb != mergedDb) {
          path(localDir, mergedDb.id).bytes = mergedDbBytes
          setHead(localDir, mergedDb.id)
        }

        if (remoteDb != mergedDb) {
          streamBytesIn(mergedDbBytes)(session.write(remoteDir, mergedDb.id.toString, _))
          streamStringIn(mergedDb.id.toString)(session.write(remoteDir, "HEAD", _))
        }

        mergedDb
      } else {
        val localHead = head(localDir)
        val localDbBytes = path(localDir, localHead).bytes
        val localDb = Database.deserialize(passphrase, localDbBytes)

        streamBytesIn(localDbBytes)(session.write(remoteDir, localDb.id.toString, _))
        streamStringIn(localDb.id.toString)(session.write(remoteDir, "HEAD", _))

        localDb
      }
    }
  }

  private def head(dir: File): UUID = UUID.fromString(new File(dir, "HEAD").text.trim)
  private def setHead(dir: File, id: UUID): Unit = new File(dir, "HEAD").text = id.toString
  private def path(dir: File, id: UUID): File = new File(dir, id.toString)

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