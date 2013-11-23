package de.choffmeister.secpwd.security

import de.choffmeister.secpwd.security.RandomGenerator._

case class PasswordCharacters(val alphaLower: Boolean = false, val alphaUpper: Boolean = false, val numbers: Boolean = false, val special: Boolean = false, val other: Boolean = false)

object PasswordCharacters {
  val alphaLowerChars = Seq[Char]('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z')
  val alphaUpperChars = Seq[Char]('A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z')
  val numbersChars = Seq[Char]('0', '1', '2', '3', '4', '5', '6', '7', '8', '9')
  val specialChars = Seq[Char]('!', '$', '%', '&', '/', '#', '=', '?', '-', '_', '.', ':', ';', ',', '*', '(', ')', '[', ']', '{', '}')

  def chars(characters: PasswordCharacters): Seq[Char] = {
    var result = Seq.empty[Char]
    if (characters.alphaLower) result = result ++ alphaLowerChars
    if (characters.alphaUpper) result = result ++ alphaUpperChars
    if (characters.numbers) result = result ++ numbersChars
    if (characters.special) result = result ++ specialChars
    result
  }

  def categorize(password: Array[Char]): PasswordCharacters = {
    var result = PasswordCharacters()
    for (c <- password) {
      if (alphaLowerChars.contains(c)) result = result.copy(alphaLower = true)
      if (alphaUpperChars.contains(c)) result = result.copy(alphaUpper = true)
      if (numbersChars.contains(c)) result = result.copy(numbers = true)
      if (specialChars.contains(c)) result = result.copy(special = true)
      if (!alphaLowerChars.contains(c) && !alphaUpperChars.contains(c) && !numbersChars.contains(c) && !specialChars.contains(c))
        result = result.copy(other = true)
    }
    result
  }
}

object PasswordUtils {
  def generate(length: Int = 32, characters: PasswordCharacters): Array[Char] = {
    val chars = PasswordCharacters.chars(characters)
    val pwd = (1 to length).map(i => chars(generateRandomInt(chars.length))).toArray

    if (PasswordCharacters.categorize(pwd) == characters) pwd
    else generate(length, characters)
  }

  def getBitEntropy(password: Array[Char]): Int = {
    var characters = PasswordCharacters.categorize(password)
    val count = PasswordCharacters.chars(characters).length
    val length = password.length

    Math.ceil(Math.log10(count) / Math.log10(2) * length).asInstanceOf[Int]
  }
}