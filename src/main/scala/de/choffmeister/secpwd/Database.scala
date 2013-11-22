package de.choffmeister.secpwd

import java.util.Date
import java.util.UUID
import java.io.{InputStream, OutputStream}
import de.choffmeister.secpwd.BinaryReaderWriter._

case class Database(
  id: UUID,
  timeStamp: Date,
  parentIds: List[UUID],
  passwords: List[PasswordEntry] = Nil
)

object Database {
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
