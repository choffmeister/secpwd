package de.choffmeister.secpwd

import java.util.UUID
import java.util.Date
import java.io.OutputStream
import java.io.InputStream
import java.nio.ByteBuffer
import java.nio.ByteOrder
import scala.language.implicitConversions
import scala.annotation.tailrec

class BinaryWriter(val stream: OutputStream) {
  private val bufRaw = new Array[Byte](8)
  private val buf = ByteBuffer.wrap(bufRaw)
  buf.order(ByteOrder.BIG_ENDIAN)

  def writeInt8(value: Byte): Unit = {
    buf.put(0, value)
    writeToStream(stream, bufRaw, 0, 1)
  }

  def writeInt16(value: Short): Unit = {
    buf.putShort(0, value)
    writeToStream(stream, bufRaw, 0, 2)
  }

  def writeInt32(value: Int): Unit = {
    buf.putInt(0, value)
    writeToStream(stream, bufRaw, 0, 4)
  }

  def writeInt64(value: Long): Unit = {
    buf.putLong(0, value)
    writeToStream(stream, bufRaw, 0, 8)
  }

  def writeBoolean(value: Boolean): Unit = value match {
    case true => stream.write(Array(1.toByte))
    case false => stream.write(Array(0.toByte))
  }

  def writeString(value: String): Unit = {
    val bytes = value.getBytes("UTF-8")
    writeInt64(bytes.length)
    writeToStream(stream, bytes, 0, bytes.length)
  }

  def writeBinary(value: Seq[Byte]): Unit = {
    writeInt64(value.length)
    writeToStream(stream, value.toArray, 0, value.length)
  }

  def writeBinary(value: Array[Byte]): Unit = {
    writeInt64(value.length)
    writeToStream(stream, value, 0, value.length)
  }

  def writeUUID(value: UUID): Unit = {
    writeInt64(value.getMostSignificantBits)
    writeInt64(value.getLeastSignificantBits)
  }

  def writeDate(value: Date): Unit = {
    writeInt64(value.getTime)
  }

  def close(): Unit = stream.close()

  private def writeToStream(stream: OutputStream, buffer: Array[Byte], offset: Int, length: Int) {
    stream.write(buffer, offset, length)
  }
}

class BinaryReader(val stream: InputStream) {
  private val bufRaw = new Array[Byte](8)
  private val buf = ByteBuffer.wrap(bufRaw)
  buf.order(ByteOrder.BIG_ENDIAN)

  def readInt8(): Byte = {
    readFromStream(stream, bufRaw, 0, 1)
    return buf.get(0)
  }

  def readInt16(): Short = {
    readFromStream(stream, bufRaw, 0, 2)
    return buf.getShort(0)
  }

  def readInt32(): Int = {
    readFromStream(stream, bufRaw, 0, 4)
    return buf.getInt(0)
  }

  def readInt64(): Long = {
    readFromStream(stream, bufRaw, 0, 8)
    return buf.getLong(0)
  }

  def readBoolean(): Boolean = {
    return readInt8() != 0
  }

  def readString(): String = {
    val length = readInt64().toInt
    val stringBuf = new Array[Byte](length)
    readFromStream(stream, stringBuf, 0, length)
    return new String(stringBuf, "UTF-8")
  }

  def readBinary(): Array[Byte] = {
    val length = readInt64().toInt
    val buf = new Array[Byte](length)
    readFromStream(stream, buf, 0, length)
    return buf
  }

  def readUUID(): UUID = {
    val a = readInt64()
    val b = readInt64()
    return new UUID(a, b)
  }

  def readDate(): Date = {
    val t = readInt64()
    return new Date(t)
  }

  def close(): Unit = stream.close()

  @tailrec
  private def readFromStream(stream: InputStream, buffer: Array[Byte], offset: Int, length: Int) {
    if (length > 0) {
      val read = stream.read(buffer, offset, length)
      readFromStream(stream, buffer, offset + read, length - read)
    }
  }
}

object BinaryReaderWriter {
  implicit def inputStreamToBinaryReader(stream: InputStream) = new BinaryReader(stream)
  implicit def outputStreamToBinaryWriter(stream: OutputStream) = new BinaryWriter(stream)
}