package de.choffmeister.secpwd

import java.util.UUID

abstract class BaseEntry {
  val id: UUID
}

case class RootEntry(
  id: UUID,
  passwords: Seq[PasswordEntry]
) extends BaseEntry

case class CustomEntry(
  id: UUID,
  key: String,
  value: String
) extends BaseEntry

case class PasswordEntry(
  id: UUID,
  key: String,
  name: String,
  password: String,
  userName: String = "",
  description: String = "",
  customFields: Seq[CustomEntry] = Nil
) extends BaseEntry
