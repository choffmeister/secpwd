package de.choffmeister.secpwd

import java.util.Date
import java.util.UUID

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
  def appendPassword(db: Database, password: PasswordEntry): Database =
    alter(db, password :: db.passwords)
  def dropPasswords(db: Database, matcher: PasswordEntry => Boolean): Database =
    alter(db, db.passwords.filter(matcher(_) == false))
  def dropPasswordById(db: Database, id: String) = dropPasswords(db, _.id == id)
  def modifyPassword(db: Database, password: PasswordEntry): Database =
    alter(db, password :: db.passwords.filter(_.id != password.id))
  def modifyPasswordValue(db: Database, id: String, password: String) =
    alter(db, db.passwords.find(_.id == id).get.copy(password = password) :: db.passwords.filter(_.id != id))

  private def now = new Date()
}

abstract class BaseEntry

case class CustomEntry(
  id: UUID,
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
