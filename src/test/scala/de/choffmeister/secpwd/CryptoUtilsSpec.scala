package de.choffmeister.secpwd

import org.specs2.mutable._
import org.specs2.runner.JUnitRunner
import org.junit.runner.RunWith
import org.apache.commons.codec.binary.Hex
import java.io._
import de.choffmeister.secpwd.BinaryReaderWriter._

@RunWith(classOf[JUnitRunner])
class CryptoUtilsSpec extends Specification {
  "derive keys" in {
    // test vectors from http://tools.ietf.org/html/rfc6070
    val key1 = CryptoUtils.deriveKey("password".toCharArray, "salt".getBytes("ASCII"), 1, 20 * 8)
    key1.toSeq === Hex.decodeHex("0c 60 c8 0f 96 1f 0e 71 f3 a9 b5 24 af 60 12 06 2f e0 37 a6".replaceAll(" ", "").toCharArray).toSeq
    val key2 = CryptoUtils.deriveKey("passwordPASSWORDpassword".toCharArray, "saltSALTsaltSALTsaltSALTsaltSALTsalt".getBytes("ASCII"), 4096, 25 * 8)
    key2.toSeq === Hex.decodeHex("3d 2e ec 4f e4 1c 84 9b 80 c8 d8 36 62 c0 e4 4a 8b 29 1a 96 4c f2 f0 70 38".replaceAll(" ", "").toCharArray).toSeq
    val key3 = CryptoUtils.deriveKey("pass\0word".toCharArray, "sa\0lt".getBytes("ASCII"), 4096, 16 * 8)
    key3.toSeq === Hex.decodeHex("56 fa 6a a7 55 48 09 9d cc 37 d7 f0 34 25 e0 c3".replaceAll(" ", "").toCharArray).toSeq
  }

  "generate random octets" in {
    val rnd1 = new Array[Byte](24)
    val rnd2 = new Array[Byte](24)
    CryptoUtils.generateRandomOctets(rnd1)
    CryptoUtils.generateRandomOctets(rnd2)
    val rnd3 = CryptoUtils.generateRandomOctets(24)
    val rnd4 = CryptoUtils.generateRandomOctets(24)

    rnd1 !== rnd2
    rnd1 !== rnd3
    rnd1 !== rnd4
    rnd2 !== rnd3
    rnd2 !== rnd4
    rnd3 !== rnd4
  }

  "encrypt and decrypt with AES" in {
    val deriveIterations = 512
    val passphrase = "secure-password".toCharArray
    val encSalt = CryptoUtils.generateRandomOctets(8)
    val macSalt = CryptoUtils.generateRandomOctets(8)
    val iv = CryptoUtils.generateRandomOctets(16)

    val plain = "Hello World! This is secpwd!"
    val encrypted = CryptoUtils.encryptAes256HmacSha512(plain.getBytes, passphrase, deriveIterations, encSalt, macSalt, iv)
    val decrypted = new String(CryptoUtils.decryptAes256HmacSha512(encrypted, passphrase, deriveIterations, encSalt, macSalt, iv))
    plain === decrypted
  }

  "fail on encryption-/decryption-key mismatch" in {
    val deriveIterations = 512
    val encSalt = CryptoUtils.generateRandomOctets(8)
    val macSalt = CryptoUtils.generateRandomOctets(8)
    val iv = CryptoUtils.generateRandomOctets(16)

    val encrypted = CryptoUtils.encryptAes256HmacSha512("Hello World!".getBytes, "secure-password1".toCharArray, deriveIterations, encSalt, macSalt, iv)
    new String(CryptoUtils.decryptAes256HmacSha512(encrypted, "secure-password2".toCharArray, deriveIterations, encSalt, macSalt, iv)) must throwA()
  }
}
