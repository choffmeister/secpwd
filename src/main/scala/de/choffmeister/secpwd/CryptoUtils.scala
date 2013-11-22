package de.choffmeister.secpwd

import java.io._
import javax.crypto._
import javax.crypto.spec._
import java.security._
import java.security.spec._

object CryptoUtils {
  private lazy val random = new SecureRandom()

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

  /**
   * Generates a cipher stream with AES/CBC/PKCS5Padding algorithm. Uses PBKDF2 to derive encryption key from pass phrase.
   */
  def encryptAes(output: OutputStream, passPhrase: Array[Char], salt: Array[Byte], iterationCount: Int, iv: Array[Byte])(inner: OutputStream => Any) {
    val cipher = Cipher.getInstance("AES/CBC/PKCS5Padding")
    val key = deriveAesKey(passPhrase, salt, iterationCount)
    cipher.init(Cipher.ENCRYPT_MODE, key, new IvParameterSpec(iv))
    val cipherStream = new CipherOutputStream(output, cipher)
    inner(cipherStream)
    cipherStream.flush()
    cipherStream.close()
  }

  /**
   * Generates a cipher stream with AES/CBC/PKCS5Padding algorithm. Uses PBKDF2 to derive encryption key from pass phrase.
   */
  def decryptAes(input: InputStream, passPhrase: Array[Char], salt: Array[Byte], iterationCount: Int, iv: Array[Byte])(inner: InputStream => Any) {
    val cipher = Cipher.getInstance("AES/CBC/PKCS5Padding")
    val key = deriveAesKey(passPhrase, salt, iterationCount)
    cipher.init(Cipher.DECRYPT_MODE, key, new IvParameterSpec(iv))
    val cipherStream = new CipherInputStream(input, cipher)
    inner(cipherStream)
    cipherStream.close()
  }

  def generateRandomOctets(length: Int): Array[Byte] = {
    val buffer = new Array[Byte](length)
    random.nextBytes(buffer)
    buffer
  }

  def generateRandomOctets(buffer: Array[Byte]) {
    random.nextBytes(buffer)
  }
}