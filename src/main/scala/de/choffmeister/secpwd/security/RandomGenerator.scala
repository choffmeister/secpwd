package de.choffmeister.secpwd.security

import java.security.SecureRandom

object RandomGenerator {
  private lazy val random = new SecureRandom()

  def generateRandomOctets(length: Int): Array[Byte] = {
    val buffer = new Array[Byte](length)
    random.nextBytes(buffer)
    buffer
  }

  def generateRandomOctets(buffer: Array[Byte]) {
    random.nextBytes(buffer)
  }

  def generateRandomInts(until: Int, length: Int) = (1 to length).map(i => generateRandomInt(until))

  def generateRandomInt(until: Int) = random.nextInt(until)
}