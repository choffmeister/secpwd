package de.choffmeister.secpwd

import java.util.UUID.randomUUID

object Main extends App {
  val pwd1 = PasswordEntry(randomUUID, "google", "Google Account", "SeCuRePwD",
    userName = "unknownuser@googlemail.com",
    customFields = Seq(
      CustomEntry(randomUUID, "Additional Info", "Valid for all my Google accounts")
    )
  )

  val pwd2 = PasswordEntry(randomUUID, "battlenet", "Battle.net", "SeCuRePwD2",
    userName = "unknownuser2@battle.net",
    description = "My gaming account at Blizzard"
  )

  val root = RootEntry(randomUUID, Seq(pwd1, pwd2))

  println(root)
}
