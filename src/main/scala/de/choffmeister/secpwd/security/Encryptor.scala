package de.choffmeister.secpwd.security

import java.io._
import javax.crypto._
import javax.crypto.spec._
import java.security._
import java.security.spec._
import de.choffmeister.secpwd.utils.BinaryReaderWriter._

object Encryptor {
  /**
   * Derives an encryption key according to PBKDF2 defined in PKCS#5 v2.0.
   * For more information see http://tools.ietf.org/html/rfc2898.
   */
  def deriveKey(passphrase: Array[Char], salt: Array[Byte], iterationCount: Int, keyLength: Int): Array[Byte] = {
    val factory = SecretKeyFactory.getInstance("PBKDF2WithHmacSHA1")
    val spec = new PBEKeySpec(passphrase, salt, iterationCount, keyLength);
    factory.generateSecret(spec).getEncoded
  }

  /**
   * Encrypts a byte array with AES/CBC/PKCS5Padding algorithm. Uses PBKDF2 to derive key from passphrase.
   */
  def encryptAes256(bytes: Array[Byte], passphrase: Array[Char], deriveIterations: Int, salt: Array[Byte], iv: Array[Byte]): Array[Byte] = {
    val cipher = Cipher.getInstance("AES/CBC/PKCS5Padding")
    val cipherKey = new SecretKeySpec(deriveKey(passphrase, salt, deriveIterations, 256), "AES")
    cipher.init(Cipher.ENCRYPT_MODE, cipherKey, new IvParameterSpec(iv))
    val encrypted = cipher.doFinal(bytes)

    encrypted
  }

  /**
   * Decrypts a byte array with AES/CBC/PKCS5Padding algorithm. Uses PBKDF2 to derive key from passphrase.
   */
  def decryptAes256(bytes: Array[Byte], passphrase: Array[Char], deriveIterations: Int, salt: Array[Byte], iv: Array[Byte]): Array[Byte] = {
    val cipher = Cipher.getInstance("AES/CBC/PKCS5Padding")
    val cipherKey = new SecretKeySpec(deriveKey(passphrase, salt, deriveIterations, 256), "AES")
    cipher.init(Cipher.DECRYPT_MODE, cipherKey, new IvParameterSpec(iv))
    val decrypted = cipher.doFinal(bytes)

    decrypted
  }

  /**
   * Signs a byte array with HMAC-SHA512. Uses PBKDF2 to derive key from passphrase.
   */
  def hmacSha512(bytes: Array[Byte], passphrase: Array[Char], deriveIterations: Int, salt: Array[Byte]): Array[Byte] = {
    val mac = Mac.getInstance("HmacSHA512")
    val macKey = new SecretKeySpec(deriveKey(passphrase, salt, deriveIterations, 512), "HmacSHA512")
    mac.init(macKey)
    val signature = mac.doFinal(bytes)

    signature
  }
}