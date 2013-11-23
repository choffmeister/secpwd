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
  def deriveKey(passphrase: Array[Char], salt: Array[Byte], iterationCount: Int, keyLength: Int): Array[Byte] = {
    val factory = SecretKeyFactory.getInstance("PBKDF2WithHmacSHA1")
    val spec = new PBEKeySpec(passphrase, salt, iterationCount, keyLength);
    factory.generateSecret(spec).getEncoded
  }

  /**
   * Generates a cipher stream with AES/CBC/PKCS5Padding algorithm. Uses PBKDF2 to derive keys from passphrase
   * and HMAC-SHA512 to ensure integrity.
   */
  def encryptAes(output: OutputStream, passphrase: Array[Char], deriveIterations: Int, aesSalt: Array[Byte], hmacSalt: Array[Byte], iv: Array[Byte])(inner: OutputStream => Any): Unit = {
    val cipher = Cipher.getInstance("AES/CBC/PKCS5Padding")
    val cipherKey = new SecretKeySpec(deriveKey(passphrase, aesSalt, deriveIterations, 256), "AES")
    cipher.init(Cipher.ENCRYPT_MODE, cipherKey, new IvParameterSpec(iv))
    val mac = Mac.getInstance("HmacSHA512")
    val macKey = new SecretKeySpec(deriveKey(passphrase, hmacSalt, deriveIterations, 512), "HmacSHA512")
    mac.init(macKey)

    val macStream = new MacOutputStream(output, mac)
    val cipherStream = new CipherOutputStream(macStream, cipher)
    inner(cipherStream)
    cipherStream.flush()
    cipherStream.close()
    macStream.flush()
    macStream.close()
    
    val signature = macStream.getSignature
    println(signature.toList)
  }

  /**
   * Generates a cipher stream with AES/CBC/PKCS5Padding algorithm. Uses PBKDF2 to derive keys from passphrase
   * and HMAC-SHA512 to ensure integrity.
   */
  def decryptAes[T](input: InputStream, passphrase: Array[Char], deriveIterations: Int, aesSalt: Array[Byte], hmacSalt: Array[Byte], iv: Array[Byte])(inner: InputStream => T): T = {
    val cipher = Cipher.getInstance("AES/CBC/PKCS5Padding")
    val cipherKey = new SecretKeySpec(deriveKey(passphrase, aesSalt, deriveIterations, 256), "AES")
    cipher.init(Cipher.DECRYPT_MODE, cipherKey, new IvParameterSpec(iv))
    val mac = Mac.getInstance("HmacSHA512")
    val macKey = new SecretKeySpec(deriveKey(passphrase, hmacSalt, deriveIterations, 512), "HmacSHA512")
    mac.init(macKey)

    val macStream = new MacInputStream(input, mac)
    val cipherStream = new CipherInputStream(macStream, cipher)
    val result = inner(cipherStream)
    cipherStream.close()
    macStream.close()

    result
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

class MacInputStream(val input: InputStream, val mac: Mac) extends InputStream {
  private var signature: Option[Array[Byte]] = None
  def getSignature: Array[Byte] = signature match {
    case Some(s) => s
    case _ => throw new Exception("MacInputStream must be closed before trying to access the signature")
  }

  override def read(): Int = {
    val result = input.read()
    if (result >= 0) {
      mac.update(result.toByte)
      result
    } else result
  }

  override def read(buffer: Array[Byte], offset: Int, length: Int): Int = {
    val result = input.read(buffer, offset, length)
    if (result >= 0) {
      mac.update(buffer, offset, length)
      result
    } else result
  }

  override def close(): Unit = {
    signature = Some(mac.doFinal())
  }
}

class MacOutputStream(val output: OutputStream, val mac: Mac) extends OutputStream {
  private var signature: Option[Array[Byte]] = None
  def getSignature: Array[Byte] = signature match {
    case Some(s) => s
    case _ => throw new Exception("MacInputStream must be closed before trying to access the signature")
  }

  override def write(byte: Int): Unit = {
    mac.update(byte.toByte)
    output.write(byte)
  }

  override def write(buffer: Array[Byte], offset: Int, length: Int): Unit = {
    mac.update(buffer, offset, length)
    output.write(buffer, offset, length)
  }

  override def close(): Unit = {
    signature = Some(mac.doFinal())
  }
}