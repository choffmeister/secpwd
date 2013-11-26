package de.choffmeister.secpwd.security

import org.specs2.mutable._
import org.specs2.runner.JUnitRunner
import org.junit.runner.RunWith
import de.choffmeister.secpwd.security.RandomGenerator._
import de.choffmeister.securestring.SecureString

@RunWith(classOf[JUnitRunner])
class PasswordUtilsSpec extends Specification {
  "generate random passwords" in {
    val pc1 = PasswordCharacters(true, false, false, false, false)
    val pwd1 = PasswordUtils.generate(10, pc1)

    val pc2 = PasswordCharacters(true, true, false, false, false)
    val pwd2 = PasswordUtils.generate(10, pc2)

    val pc3 = PasswordCharacters(true, true, true, false, false)
    val pwd3 = PasswordUtils.generate(10, pc3)

    val pc4 = PasswordCharacters(true, true, true, true, false)
    val pwd4 = PasswordUtils.generate(10, pc4)

    val pc5 = PasswordCharacters(true, true, true, true, true)
    val pwd5 = PasswordUtils.generate(10, pc5)

    pwd1 !== pwd2
    pwd1 !== pwd3
    pwd1 !== pwd4
    pwd1 !== pwd5
    pwd2 !== pwd3
    pwd2 !== pwd4
    pwd2 !== pwd5
    pwd3 !== pwd4
    pwd3 !== pwd5
    pwd4 !== pwd5
  }

  "calculate entropy" in {
    PasswordUtils.getBitEntropy(SecureString("abcdefghij".toCharArray)) === 48
    PasswordUtils.getBitEntropy(SecureString("Abcdefghij".toCharArray)) === 58
    PasswordUtils.getBitEntropy(SecureString("A1cdefghij".toCharArray)) === 60
    PasswordUtils.getBitEntropy(SecureString("A1#defghij".toCharArray)) === 64
    PasswordUtils.getBitEntropy(SecureString("A1#'efghij".toCharArray)) === 64
    ok
  }

  "categorize" in {
    PasswordCharacters.categorize(SecureString("abcdefghij".toCharArray)) === PasswordCharacters(true, false, false, false, false)
    PasswordCharacters.categorize(SecureString("Abcdefghij".toCharArray)) === PasswordCharacters(true, true, false, false, false)
    PasswordCharacters.categorize(SecureString("A1cdefghij".toCharArray)) === PasswordCharacters(true, true, true, false, false)
    PasswordCharacters.categorize(SecureString("A1#defghij".toCharArray)) === PasswordCharacters(true, true, true, true, false)
    PasswordCharacters.categorize(SecureString("A1#'efghij".toCharArray)) === PasswordCharacters(true, true, true, true, true)
    ok
  }
}
