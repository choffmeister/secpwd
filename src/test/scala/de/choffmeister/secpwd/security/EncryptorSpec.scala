package de.choffmeister.secpwd.security

import org.specs2.mutable._
import org.specs2.runner.JUnitRunner
import org.junit.runner.RunWith
import java.io._
import org.apache.commons.codec.binary.Hex
import de.choffmeister.secpwd.utils.BinaryReaderWriter._
import de.choffmeister.secpwd.security.Encryptor._
import de.choffmeister.secpwd.security.RandomGenerator._

@RunWith(classOf[JUnitRunner])
class EncryptorSpec extends Specification {
  "derive keys" in {
    // test vectors from http://tools.ietf.org/html/rfc6070
    val key1 = deriveKey("password".toCharArray, "salt".getBytes("ASCII"), 1, 20 * 8)
    key1.toSeq === Hex.decodeHex("0c 60 c8 0f 96 1f 0e 71 f3 a9 b5 24 af 60 12 06 2f e0 37 a6".replaceAll(" ", "").toCharArray).toSeq
    val key2 = deriveKey("passwordPASSWORDpassword".toCharArray, "saltSALTsaltSALTsaltSALTsaltSALTsalt".getBytes("ASCII"), 4096, 25 * 8)
    key2.toSeq === Hex.decodeHex("3d 2e ec 4f e4 1c 84 9b 80 c8 d8 36 62 c0 e4 4a 8b 29 1a 96 4c f2 f0 70 38".replaceAll(" ", "").toCharArray).toSeq
    val key3 = deriveKey("pass\0word".toCharArray, "sa\0lt".getBytes("ASCII"), 4096, 16 * 8)
    key3.toSeq === Hex.decodeHex("56 fa 6a a7 55 48 09 9d cc 37 d7 f0 34 25 e0 c3".replaceAll(" ", "").toCharArray).toSeq
  }

  "encrypt and decrypt with AES" in {
    val deriveIterations = 512
    val passphrase = "secure-password".toCharArray
    val encSalt = generateRandomOctets(8)
    val macSalt = generateRandomOctets(8)
    val iv = generateRandomOctets(16)

    val plain = "Hello World! This is secpwd!"
    val encrypted = encryptAes256HmacSha512(plain.getBytes, passphrase, deriveIterations, encSalt, macSalt, iv)
    val decrypted = new String(decryptAes256HmacSha512(encrypted, passphrase, deriveIterations, encSalt, macSalt, iv))
    plain === decrypted
  }

  "fail on encryption-/decryption-key mismatch" in {
    val deriveIterations = 512
    val encSalt = generateRandomOctets(8)
    val macSalt = generateRandomOctets(8)
    val iv = generateRandomOctets(16)

    val encrypted = encryptAes256HmacSha512("Hello World!".getBytes, "secure-password1".toCharArray, deriveIterations, encSalt, macSalt, iv)
    new String(decryptAes256HmacSha512(encrypted, "secure-password2".toCharArray, deriveIterations, encSalt, macSalt, iv)) must throwA()
  }
}
