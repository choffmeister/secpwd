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

case class Database(
  id: UUID,
  timeStamp: Date,
  parentIds: List[UUID],
  passwords: List[PasswordEntry] = Nil
)

case class DatabaseCryptoInfo(deriveIterations: Int, macSalt: Array[Byte], encSalt: Array[Byte], iv: Array[Byte])

class DatabaseSerializationException(message: String) extends Exception(message)

object Database {
  val MAGIC_BYTES = Array[Byte](115, 99, 112, 100)

  def create(): Database = Database(UUID.randomUUID, now, Nil)
  def alter(db: Database, passwords: List[PasswordEntry]): Database =
    db.copy(id = UUID.randomUUID, timeStamp = now, parentIds = List(db.id), passwords = passwords)
  def addPassword(db: Database, password: PasswordEntry): Database =
    alter(db, password :: db.passwords)
  def removePasswordById(db: Database, id: String) = 
    alter(db, db.passwords.filter(_.id != id))
  def updatePassword(db: Database, password: PasswordEntry): Database =
    alter(db, password :: db.passwords.filter(_.id != password.id))
  def updatePassword(db: Database, id: String, password: String) =
    alter(db, db.passwords.find(_.id == id).get.copy(password = password) :: db.passwords.filter(_.id != id))

  private def now = new Date()

  def serializeDatabase(db: Database): Array[Byte] = {
    val tmp = new ByteArrayOutputStream()
    serializeDatabase(tmp, db)
    tmp.toByteArray
  }

  def serializeDatabase(output: OutputStream, db: Database): Unit = {
    output.writeUUID(db.id)
    output.writeDate(db.timeStamp)
    output.writeInt32(db.parentIds.length)
    db.parentIds.foreach(output.writeUUID(_))
    output.writeInt32(db.passwords.length)
    db.passwords.foreach(serializePasswordEnty(output, _))
  }

  def serializePasswordEnty(output: OutputStream, pwd: PasswordEntry): Unit = {
    output.writeString(pwd.id)
    output.writeDate(pwd.timeStamp)
    output.writeString(pwd.name)
    output.writeString(pwd.password)
    output.writeString(pwd.userName)
    output.writeString(pwd.description)
    output.writeInt32(pwd.customFields.length)
    pwd.customFields.foreach(serializeCustomEntry(output, _))
  }

  def serializeCustomEntry(output: OutputStream, cf: CustomEntry): Unit = {
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
      (1 to input.readInt32()).map(i => input.readUUID()).toList,
      (1 to input.readInt32()).map(i => deserializePasswordEntry(input)).toList
    )
  }

  def deserializePasswordEntry(input: InputStream): PasswordEntry = {
    PasswordEntry(
      input.readString(),
      input.readDate(),
      input.readString(),
      input.readString(),
      input.readString(),
      input.readString(),
      (1 to input.readInt32()).map(i => deserializeCustomEntry(input)).toList
    )
  }

  def deserializeCustomEntry(input: InputStream): CustomEntry = {
    CustomEntry(
      input.readString(),
      input.readString()
    )
  }

  def serialize(passphrase: Array[Char], path: File, db: Database): Unit = {
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
      ms.writeBinary(encryptAes256(serializeDatabase(db), passphrase, cryptinfo.deriveIterations, cryptinfo.encSalt, cryptinfo.iv))
    }

    bs.writeBytesRaw(hmacSha512(bs.toByteArray, passphrase, cryptinfo.deriveIterations, cryptinfo.macSalt))

    val allBytes = bs.toByteArray
    path.bytes = allBytes
  }

  def deserialize(passphrase: Array[Char], path: File): Database = {
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

    deserializeDatabase(decryptAes256(encrypted, passphrase, cryptoinfo.deriveIterations, cryptoinfo.encSalt, cryptoinfo.iv))
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

abstract class BaseEntry

case class CustomEntry(
  key: String,
  value: String
) extends BaseEntry

case class PasswordEntry(
  id: String,
  timeStamp: Date,
  name: String,
  password: String,
  userName: String = "",
  description: String = "",
  customFields: List[CustomEntry] = Nil
) extends BaseEntry
