package de.choffmeister.secpwd

import java.util.Date
import java.util.UUID
import java.io.{InputStream, OutputStream, ByteArrayInputStream, ByteArrayOutputStream}
import java.io.{File, FileInputStream, FileOutputStream}
import de.choffmeister.secpwd.utils.BinaryReaderWriter._
import de.choffmeister.secpwd.utils.RichFile._
import de.choffmeister.secpwd.utils.RichStream._
import de.choffmeister.secpwd.security.Encryptor._
import de.choffmeister.secpwd.security.RandomGenerator._
import de.choffmeister.securestring.SecureString

abstract class BaseEntry {
  val id: UUID
}

case class Database(
  id: UUID,
  timeStamp: Date,
  versions: List[DatabaseVersion],
  passwords: List[PasswordEntry] = Nil
) extends BaseEntry {
  def passwordById(id: UUID): Option[PasswordEntry] =
    passwords.find(_.id == id)

  def currentPasswordByKey(key: String): Option[PasswordEntry] =
    versions(0).passwordIds.map(passwordById(_).get).find(_.key == key)

  def currentPasswords: List[PasswordEntry] =
    versions(0).passwordIds.map(passwordById(_).get)
}

case class DatabaseVersion(
  versionId: UUID,
  timeStamp: Date,
  parentVersionIds: List[UUID] = Nil,
  passwordIds: List[UUID] = Nil
)

case class CustomEntry(
  id: UUID,
  key: String,
  value: String
) extends BaseEntry

case class PasswordEntry(
  id: UUID,
  timeStamp: Date,
  key: String,
  name: String,
  password: SecureString,
  userName: String = "",
  description: String = "",
  customFields: List[CustomEntry] = Nil
)

case class DatabaseCryptoInfo(deriveIterations: Int, macSalt: Array[Byte], encSalt: Array[Byte], iv: Array[Byte])

class DatabaseSerializationException(message: String) extends Exception(message)

object Database {
  val MAGIC_BYTES = Array[Byte](115, 99, 112, 100)

  def create(): Database = {
    val id = UUID.randomUUID()
    val now = new Date()
    Database(id, now, List(DatabaseVersion(id, now)))
  }

  def addPassword(db: Database, password: PasswordEntry): Database =
    db.currentPasswordByKey(password.key) match {
      case Some(pwd) => throw new Exception()
      case _ => alter(db, password.id :: db.versions(0).passwordIds, password :: db.passwords)
    }

  def removePasswordById(db: Database, id: UUID): Database =
    db.passwordById(id) match {
      case Some(pwd) => alter(db, db.versions(0).passwordIds.filter(_ != id), db.passwords)
      case _ => throw new Exception()
    }

  def removePasswordByKey(db: Database, key: String): Database =
    db.currentPasswordByKey(key) match {
      case Some(pwd) => removePasswordById(db, pwd.id)
      case _ => throw new Exception()
    }

  def updatePassword(db: Database, password: PasswordEntry): Database =
    db.currentPasswordByKey(password.key) match {
      case Some(pwd) => alter(db, password.id :: db.versions(0).passwordIds.filter(_ != pwd.id), password :: db.passwords)
      case _ => throw new Exception()
    }

  def updatePassword(db: Database, key: String, password: SecureString): Database =
    db.currentPasswordByKey(key) match {
      case Some(pwd) => updatePassword(db, pwd.copy(id = UUID.randomUUID(), password = password))
      case _ => throw new Exception()
    }

  private def alter(db: Database, passwordId: List[UUID], passwords: List[PasswordEntry]): Database = {
    val id = UUID.randomUUID()
    val now = new Date()
    db.copy(id = id, timeStamp = now, versions = DatabaseVersion(id, now, List(db.id), passwordId) :: db.versions, passwords = passwords)
  }

  def serializeDatabase(db: Database): Array[Byte] = {
    val tmp = new ByteArrayOutputStream()
    serializeDatabase(tmp, db)
    tmp.toByteArray
  }

  def serializeDatabase(output: OutputStream, db: Database): Unit = {
    output.writeUUID(db.id)
    output.writeDate(db.timeStamp)
    output.writeInt32(db.versions.length)
    db.versions.foreach(serializeDatabaseVersion(output, _))
    output.writeInt32(db.passwords.length)
    db.passwords.foreach(serializePasswordEnty(output, _))
  }

  def serializeDatabaseVersion(output: OutputStream, version: DatabaseVersion): Unit = {
    output.writeUUID(version.versionId)
    output.writeDate(version.timeStamp)
    output.writeInt32(version.parentVersionIds.length)
    version.parentVersionIds.foreach(output.writeUUID(_))
    output.writeInt32(version.passwordIds.length)
    version.passwordIds.foreach(output.writeUUID(_))
  }
  
  def serializePasswordEnty(output: OutputStream, pwd: PasswordEntry): Unit = {
    output.writeUUID(pwd.id)
    output.writeDate(pwd.timeStamp)
    output.writeString(pwd.key)
    output.writeString(pwd.name)
    output.writeSecureString(pwd.password)
    output.writeString(pwd.userName)
    output.writeString(pwd.description)
    output.writeInt32(pwd.customFields.length)
    pwd.customFields.foreach(serializeCustomEntry(output, _))
  }

  def serializeCustomEntry(output: OutputStream, cf: CustomEntry): Unit = {
    output.writeUUID(cf.id)
    output.writeString(cf.key)
    output.writeString(cf.value)
  }

  def deserializeDatabase(bytes: Array[Byte]): Database = {
    val tmp = new ByteArrayInputStream(bytes)
    deserializeDatabase(tmp)
  }
  
  def deserializeDatabase(input: InputStream): Database = {
    Database(
      input.readUUID(),
      input.readDate(),
      (1 to input.readInt32()).map(i => deserializeDatabaseVersion(input)).toList,
      (1 to input.readInt32()).map(i => deserializePasswordEntry(input)).toList
    )
  }

  def deserializeDatabaseVersion(input: InputStream): DatabaseVersion = {
    DatabaseVersion(
      input.readUUID(),
      input.readDate(),
      (1 to input.readInt32()).map(i => input.readUUID()).toList,
      (1 to input.readInt32()).map(i => input.readUUID()).toList
    )
  }

  def deserializePasswordEntry(input: InputStream): PasswordEntry = {
    PasswordEntry(
      input.readUUID(),
      input.readDate(),
      input.readString(),
      input.readString(),
      input.readSecureString(),
      input.readString(),
      input.readString(),
      (1 to input.readInt32()).map(i => deserializeCustomEntry(input)).toList
    )
  }

  def deserializeCustomEntry(input: InputStream): CustomEntry = {
    CustomEntry(
      input.readUUID(),
      input.readString(),
      input.readString()
    )
  }

  def serialize(passphrase: SecureString, path: File, db: Database): Unit = {
    val cryptinfo = DatabaseCryptoInfo(1024 * 16, generateRandomOctets(128), generateRandomOctets(128), generateRandomOctets(16))
    val bs = new ByteArrayOutputStream()

    bs.writeBytesRaw(MAGIC_BYTES)

    writeBlock(bs) { ms =>
      ms.writeInt32(cryptinfo.deriveIterations)
      ms.writeBinary(cryptinfo.macSalt)
      ms.writeBinary(cryptinfo.encSalt)
      ms.writeBinary(cryptinfo.iv)
    }

    writeBlock(bs) { ms =>
      ms.writeBinary(encryptAes128(serializeDatabase(db), passphrase, cryptinfo.deriveIterations, cryptinfo.encSalt, cryptinfo.iv))
    }

    bs.writeBytesRaw(hmacSha512(bs.toByteArray, passphrase, cryptinfo.deriveIterations, cryptinfo.macSalt))

    val allBytes = bs.toByteArray
    path.bytes = allBytes
  }

  def deserialize(passphrase: SecureString, path: File): Database = {
    val allBytes = path.bytes
    val bs = new ByteArrayInputStream(allBytes)

    val magicbytes = bs.readBytesRaw(4)
    if (!compareByteArrays(magicbytes, MAGIC_BYTES)) throw new DatabaseSerializationException("Invalid magic bytes")

    val cryptoinfo = readBlock(bs) { ms =>
      DatabaseCryptoInfo(
        ms.readInt32(),
        ms.readBinary(),
        ms.readBinary(),
        ms.readBinary()
      )
    }

    val encrypted = readBlock(bs) { ms =>
      ms.readBinary()
    }

    val signature = bs.readBytesRaw(64)

    val calcSignature = hmacSha512(allBytes.take(allBytes.length - 64), passphrase, cryptoinfo.deriveIterations, cryptoinfo.macSalt)
    if (!compareByteArrays(calcSignature, signature)) throw new DatabaseSerializationException("Invalid passphrase")

    deserializeDatabase(decryptAes128(encrypted, passphrase, cryptoinfo.deriveIterations, cryptoinfo.encSalt, cryptoinfo.iv))
  }

  def writeBlock(output: OutputStream)(inner: OutputStream => Any): Unit = {
    output.cached(cs => output.writeInt32(cs.size)) { cs =>
      inner(cs)
    }
  }

  def readBlock[T](stream: InputStream)(inner: InputStream => T): T = {
    var result: Option[T] = None
    stream.preSizedInner(stream.readInt32()) { is =>
      result = Some(inner(is))
    }
    return result.get
  }

  def compareByteArrays(arr1: Array[Byte], arr2: Array[Byte]): Boolean = {
    compareByteArrayChunks(arr1, 0, arr2, 0, Math.max(arr1.length, arr2.length))
  }

  def compareByteArrayChunks(arr1: Array[Byte], off1: Int, arr2: Array[Byte], off2: Int, len: Int): Boolean = {
    if (len == 0) true
    else if (arr1.length <= off1 || arr2.length <= off2) false
    else if (arr1(off1) != arr2(off2)) false
    else compareByteArrayChunks(arr1, off1 + 1, arr2, off2 + 1, len - 1)
  }
}