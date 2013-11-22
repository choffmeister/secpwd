package de.choffmeister.secpwd

import java.util.Date
import java.util.UUID.randomUUID

object Main extends App {
  val pwd1 = PasswordEntry("google", now, "Google Account", "SeCuRePwD",
    userName = "unknownuser@googlemail.com",
    customFields = List(
      CustomEntry(randomUUID, "Additional Info", "Valid for all my Google accounts")
    )
  )

  val pwd2 = PasswordEntry("battlenet", now, "Battle.net", "SeCuRePwD2",
    userName = "unknownuser2@battle.net",
    description = "My gaming account at Blizzard"
  )

  val db1 = Database.create()
  println("DB1: " + db1)
  val db2 = Database.appendPassword(db1, pwd1)
  println("DB2: " + db2)
  val db3 = Database.appendPassword(db2, pwd2)
  println("DB3: " + db3)
  val db4 = Database.dropPasswordById(db3, pwd1.id)
  println("DB4: " + db4)
  val db5 = Database.modifyPasswordValue(db4, "battlenet", "APPLE")
  println("DB5: " + db5)

  def now: Date = new Date()
}
