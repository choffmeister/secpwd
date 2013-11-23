package de.choffmeister.secpwd

import java.util.Date
import java.util.UUID
import java.io.{InputStream, OutputStream, ByteArrayInputStream, ByteArrayOutputStream}
import java.io.{File, FileInputStream, FileOutputStream}
import de.choffmeister.secpwd.utils.BinaryReaderWriter._
import de.choffmeister.secpwd.security.Encryptor._
import de.choffmeister.secpwd.security.RandomGenerator._

case class Database(
  id: UUID,
  timeStamp: Date,
  parentIds: List[UUID],
  passwords: List[PasswordEntry] = Nil
)

object Database {
  val magicBytes = 1935896676

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
    val fs = new FileOutputStream(path)

    try {
      val deriveIterations = 1024 * 16
      val encSalt = generateRandomOctets(128)
      val macSalt = generateRandomOctets(128)
      val iv = generateRandomOctets(16)

      fs.writeInt32(magicBytes)
      fs.writeInt32(deriveIterations)
      fs.writeBinary(encSalt)
      fs.writeBinary(macSalt)
      fs.writeBinary(iv)
      fs.writeBinary(encryptAes256HmacSha512(serializeDatabase(db), passphrase, deriveIterations, encSalt, macSalt, iv))
    } finally {
      fs.close()
    }
  }

  def deserialize(passphrase: Array[Char], path: File): Database = {
    val fs = new FileInputStream(path)

    try {
      // TODO: check magic bytes
      val mb = fs.readInt32()
      val deriveIterations = fs.readInt32()
      val encSalt = fs.readBinary()
      val macSalt = fs.readBinary()
      val iv = fs.readBinary()
      deserializeDatabase(decryptAes256HmacSha512(fs.readBinary(), passphrase, deriveIterations, encSalt, macSalt, iv))
    } finally {
      fs.close()
    }
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
