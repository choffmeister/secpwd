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
   * Encrypts a byte array with AES/CBC/PKCS5Padding algorithm. Uses PBKDF2 to derive keys from passphrase
   * and HMAC-SHA512 to ensure integrity.
   */
  def encryptAes256HmacSha512(bytes: Array[Byte], passphrase: Array[Char], deriveIterations: Int, aesSalt: Array[Byte], hmacSalt: Array[Byte], iv: Array[Byte]): Array[Byte] = {
    val cipher = Cipher.getInstance("AES/CBC/PKCS5Padding")
    val cipherKey = new SecretKeySpec(deriveKey(passphrase, aesSalt, deriveIterations, 256), "AES")
    cipher.init(Cipher.ENCRYPT_MODE, cipherKey, new IvParameterSpec(iv))
    val encrypted = cipher.update(bytes, 0, bytes.length) ++ cipher.doFinal()

    val mac = Mac.getInstance("HmacSHA512")
    val macKey = new SecretKeySpec(deriveKey(passphrase, hmacSalt, deriveIterations, 512), "HmacSHA512")
    mac.init(macKey)
    mac.update(encrypted, 0, encrypted.length)
    val signature = mac.doFinal()

    encrypted ++ signature
  }

  /**
   * Decrypts a byte array with AES/CBC/PKCS5Padding algorithm. Uses PBKDF2 to derive keys from passphrase
   * and HMAC-SHA512 to ensure integrity.
   */
  def decryptAes256HmacSha512(bytes: Array[Byte], passphrase: Array[Char], deriveIterations: Int, aesSalt: Array[Byte], hmacSalt: Array[Byte], iv: Array[Byte]): Array[Byte] = {
    val mac = Mac.getInstance("HmacSHA512")
    val macKey = new SecretKeySpec(deriveKey(passphrase, hmacSalt, deriveIterations, 512), "HmacSHA512")
    mac.init(macKey)
    mac.update(bytes, 0, bytes.length - 64)
    val signature = mac.doFinal()

    if (!compareByteArrayChunks(bytes, bytes.length - 64, signature, 0, 64)) throw new Exception("Passphrase invalid")

    val cipher = Cipher.getInstance("AES/CBC/PKCS5Padding")
    val cipherKey = new SecretKeySpec(deriveKey(passphrase, aesSalt, deriveIterations, 256), "AES")
    cipher.init(Cipher.DECRYPT_MODE, cipherKey, new IvParameterSpec(iv))
    val decrypted = cipher.update(bytes, 0, bytes.length - 64) ++ cipher.doFinal()

    decrypted
  }

  private def compareByteArrayChunks(arr1: Array[Byte], off1: Int, arr2: Array[Byte], off2: Int, len: Int): Boolean = {
    if (len == 0) true
    else if (arr1.length <= off1 || arr2.length <= off2) false
    else if (arr1(off1) != arr2(off2)) false
    else compareByteArrayChunks(arr1, off1 + 1, arr2, off2 + 1, len - 1)
  }
}