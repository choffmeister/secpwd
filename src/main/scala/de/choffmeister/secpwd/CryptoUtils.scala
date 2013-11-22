package de.choffmeister.secpwd

import javax.crypto._
import javax.crypto.spec._
import java.security._
import java.security.spec._

object CryptoUtils {
  /**
   * Derives an encryption key according to PBKDF2 defined in PKCS#5 v2.0.
   * For more information see http://tools.ietf.org/html/rfc2898.
   */
  def deriveKey(passPhrase: Array[Char], salt: Array[Byte], iterationCount: Int, keyLength: Int): Array[Byte] = {
    val factory = SecretKeyFactory.getInstance("PBKDF2WithHmacSHA1")
    val spec = new PBEKeySpec(passPhrase, salt, iterationCount, keyLength);
    factory.generateSecret(spec).getEncoded
  }

  /**
   * Derives an encryption key according to PBKDF2 defined in PKCS#5 v2.0.
   * For more information see http://tools.ietf.org/html/rfc2898.
   */
  def deriveAesKey(passPhrase: Array[Char], salt: Array[Byte], iterationCount: Int = 1024 * 128): SecretKeySpec = {
    new SecretKeySpec(deriveKey(passPhrase, salt, iterationCount, 256), "AES")
  }
}