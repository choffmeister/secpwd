package de.choffmeister.secpwd.security

import org.specs2.mutable._
import org.specs2.runner.JUnitRunner
import org.junit.runner.RunWith
import java.io._
import javax.crypto.BadPaddingException
import org.apache.commons.codec.binary.Hex
import de.choffmeister.secpwd.utils.BinaryReaderWriter._
import de.choffmeister.secpwd.security.Encryptor._
import de.choffmeister.secpwd.security.RandomGenerator._
import de.choffmeister.securestring.SecureString

@RunWith(classOf[JUnitRunner])
class EncryptorSpec extends Specification {
  "derive keys" in {
    // test vectors from http://tools.ietf.org/html/rfc6070
    val key1 = deriveKey(SecureString("password".toCharArray), "salt".getBytes("ASCII"), 1, 20 * 8)
    key1.toSeq === Hex.decodeHex("0c 60 c8 0f 96 1f 0e 71 f3 a9 b5 24 af 60 12 06 2f e0 37 a6".replaceAll(" ", "").toCharArray).toSeq
    val key2 = deriveKey(SecureString("passwordPASSWORDpassword".toCharArray), "saltSALTsaltSALTsaltSALTsaltSALTsalt".getBytes("ASCII"), 4096, 25 * 8)
    key2.toSeq === Hex.decodeHex("3d 2e ec 4f e4 1c 84 9b 80 c8 d8 36 62 c0 e4 4a 8b 29 1a 96 4c f2 f0 70 38".replaceAll(" ", "").toCharArray).toSeq
    val key3 = deriveKey(SecureString("pass\0word".toCharArray), "sa\0lt".getBytes("ASCII"), 4096, 16 * 8)
    key3.toSeq === Hex.decodeHex("56 fa 6a a7 55 48 09 9d cc 37 d7 f0 34 25 e0 c3".replaceAll(" ", "").toCharArray).toSeq
  }

  "encrypt and decrypt with AES-128" in {
    val deriveIterations = 512
    val passphrase = SecureString("secure-password".toCharArray)
    val salt = generateRandomOctets(8)
    val iv = generateRandomOctets(16)

    val plain = "Hello World! This is secpwd!"
    val encrypted = encryptAes(plain.getBytes, passphrase, deriveIterations, 128, salt, iv)
    val decrypted = new String(decryptAes(encrypted, passphrase, deriveIterations, 128, salt, iv))
    decrypted === plain
  }

  "fail on encryption-/decryption-key mismatch" in {
    try {
      val deriveIterations = 512
      val salt = generateRandomOctets(8)
      val iv = generateRandomOctets(16)

      val plain = "Hello World! This is secpwd!"
      val encrypted = encryptAes(plain.getBytes, SecureString("secure-password1".toCharArray), deriveIterations, 128, salt, iv)
      val decrypted = new String(decryptAes(encrypted, SecureString("secure-password2".toCharArray), deriveIterations, 128, salt, iv))
      decrypted !== plain
    } catch {
      case e: BadPaddingException => ok
    }
  }

  "mac with HMAC-SHA-512" in {
    val deriveIterations = 512
    val salt = generateRandomOctets(8)
    val iv = generateRandomOctets(16)

    val signature1 = hmacSha512("Hello World!".getBytes, SecureString("secure-password1".toCharArray), deriveIterations, salt)
    val signature2 = hmacSha512("Hello World2!".getBytes, SecureString("secure-password1".toCharArray), deriveIterations, salt)
    val signature3 = hmacSha512("Hello World!".getBytes, SecureString("secure-password2".toCharArray), deriveIterations, salt)
    val signature4 = hmacSha512("Hello World2!".getBytes, SecureString("secure-password2".toCharArray), deriveIterations, salt)
    signature1 !== signature2
    signature1 !== signature3
    signature1 !== signature4
    signature2 !== signature3
    signature2 !== signature4
    signature3 !== signature4
  }
}
