package de.choffmeister.secpwd

import org.specs2.mutable._
import org.specs2.runner.JUnitRunner
import org.junit.runner.RunWith
import org.apache.commons.codec.binary.Hex
import java.io._

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

  "derive AES keys" in {
    val key = CryptoUtils.deriveAesKey("super-secret".toCharArray, "epic-random".getBytes("ASCII"))
    key.getEncoded must haveSize(256 / 8)
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
    val passphrase = "secure-password".toCharArray
    val salt = CryptoUtils.generateRandomOctets(8)
    val iterationCount = 512
    val iv = CryptoUtils.generateRandomOctets(16)

    val encryptedWrite = new ByteArrayOutputStream()
    CryptoUtils.encryptAes(encryptedWrite, passphrase, salt, iterationCount, iv)(writeString(_, "Hello World!"))
    val encryptedRead = new ByteArrayInputStream(encryptedWrite.toByteArray)
    CryptoUtils.decryptAes(encryptedRead, passphrase, salt, iterationCount, iv)(readString(_) === "Hello World!")

    ok
  }

  "fail on encryption-/decryption-key mismatch" in {
    val salt = CryptoUtils.generateRandomOctets(8)
    val iterationCount = 512
    val iv = CryptoUtils.generateRandomOctets(16)

    val encryptedWrite = new ByteArrayOutputStream()
    CryptoUtils.encryptAes(encryptedWrite, "secure-password1".toCharArray, salt, iterationCount, iv)(writeString(_, "Hello World!"))
    val encryptedRead = new ByteArrayInputStream(encryptedWrite.toByteArray)
    CryptoUtils.decryptAes(encryptedRead, "secure-password2".toCharArray, salt, iterationCount, iv)(readString(_) !== "Hello World!")

    ok
  }

  def writeString(s: OutputStream, str: String) {
    s.write(str.getBytes("UTF-8"))
  }

  def readString(s: InputStream): String = {
    val temp = new ByteArrayOutputStream()
    val buffer = new Array[Byte](1024)
    var done = false

    while (!done) {
      val read = s.read(buffer, 0, buffer.length)
      if (read > 0) temp.write(buffer, 0, read) else done = true
    }

    new String(temp.toByteArray, "UTF-8")
  }
}
