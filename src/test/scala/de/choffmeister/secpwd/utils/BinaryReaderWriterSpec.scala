package de.choffmeister.secpwd.utils

import org.specs2.mutable._
import org.specs2.runner.JUnitRunner
import org.junit.runner.RunWith
import java.util.{UUID, Date}
import java.io.{ByteArrayInputStream, ByteArrayOutputStream}

@RunWith(classOf[JUnitRunner])
class BinaryReaderWriterSpec extends Specification {
  "BinaryReaderWriter" should {
    "use big endian (network byte order)" in {
      val streamWrite = new ByteArrayOutputStream()
      val writer = new BinaryWriter(streamWrite)

      writer.writeInt8(1)
      writer.writeInt16(16385)
      writer.writeInt32(134480385)
      writer.writeInt64(9169364094552375809L)
      
      val buf = streamWrite.toByteArray()
      buf.length === 15
      
      buf(0) === 1.toByte
      
      buf(1) === 64.toByte
      buf(2) === 1.toByte
      
      buf(3) === 8.toByte
      buf(4) === 4.toByte
      buf(5) === 2.toByte
      buf(6) === 1.toByte
      
      buf(7) === 127.toByte
      buf(8) === 64.toByte
      buf(9) === 32.toByte
      buf(10) === 16.toByte
      buf(11) === 8.toByte
      buf(12) === 4.toByte
      buf(13) === 2.toByte
      buf(14) === 1.toByte
    }

    "read and write Int8" in {
      val streamWrite = new ByteArrayOutputStream()
      val writer = new BinaryWriter(streamWrite)

      writer.writeInt8(Byte.MinValue)
      writer.writeInt8(1.toByte)
      writer.writeInt8(Byte.MaxValue)
      writer.close()

      val buf = streamWrite.toByteArray()
      buf.length === 3

      val streamRead = new ByteArrayInputStream(buf)
      val reader = new BinaryReader(streamRead)

      reader.readInt8() === Byte.MinValue
      reader.readInt8() === 1.toByte
      reader.readInt8() === Byte.MaxValue
    }

    "read and write Int16" in {
      val streamWrite = new ByteArrayOutputStream()
      val writer = new BinaryWriter(streamWrite)

      writer.writeInt16(Short.MinValue)
      writer.writeInt16(16385.toShort)
      writer.writeInt16(Short.MaxValue)
      writer.close()

      val buf = streamWrite.toByteArray()
      buf.length === 6

      val streamRead = new ByteArrayInputStream(buf)
      val reader = new BinaryReader(streamRead)

      reader.readInt16() === Short.MinValue
      reader.readInt16() === 16385.toShort
      reader.readInt16() === Short.MaxValue
    }

    "read and write Int32" in {
      val streamWrite = new ByteArrayOutputStream()
      val writer = new BinaryWriter(streamWrite)

      writer.writeInt32(Int.MinValue)
      writer.writeInt32(134480385)
      writer.writeInt32(Int.MaxValue)
      writer.close()

      val buf = streamWrite.toByteArray()
      buf.length === 12

      val streamRead = new ByteArrayInputStream(buf)
      val reader = new BinaryReader(streamRead)

      reader.readInt32() === Int.MinValue
      reader.readInt32() === 134480385
      reader.readInt32() === Int.MaxValue
    }

    "read and write Int64" in {
      val streamWrite = new ByteArrayOutputStream()
      val writer = new BinaryWriter(streamWrite)

      writer.writeInt64(Long.MinValue)
      writer.writeInt64(9169364094552375809L)
      writer.writeInt64(Long.MaxValue)
      writer.close()

      val buf = streamWrite.toByteArray()
      buf.length === 24

      val streamRead = new ByteArrayInputStream(buf)
      val reader = new BinaryReader(streamRead)

      reader.readInt64() === Long.MinValue
      reader.readInt64() === 9169364094552375809L
      reader.readInt64() === Long.MaxValue
    }

    "read and write Boolean" in {
      val streamWrite = new ByteArrayOutputStream()
      val writer = new BinaryWriter(streamWrite)

      writer.writeBoolean(true)
      writer.writeBoolean(false)
      writer.close()

      val buf = streamWrite.toByteArray()
      buf.length === 2

      val streamRead = new ByteArrayInputStream(buf)
      val reader = new BinaryReader(streamRead)

      reader.readBoolean() === true
      reader.readBoolean() === false
    }

    "read and write String" in {
      val streamWrite = new ByteArrayOutputStream()
      val writer = new BinaryWriter(streamWrite)

      writer.writeString("foo")
      writer.writeString("")
      writer.writeString("äöü")
      writer.close()

      val buf = streamWrite.toByteArray()
      buf.length === 33

      val streamRead = new ByteArrayInputStream(buf)
      val reader = new BinaryReader(streamRead)

      reader.readString() === "foo"
      reader.readString() === ""
      reader.readString() === "äöü"
    }

    "read and write Binary" in {
      val streamWrite = new ByteArrayOutputStream()
      val writer = new BinaryWriter(streamWrite)

      writer.writeBinary(Array(Byte.MinValue, Byte.MaxValue))
      writer.writeBinary(Array.empty[Byte])
      writer.writeBinary(Array(10.toByte, 11.toByte, 12.toByte))
      writer.close()

      val buf = streamWrite.toByteArray()
      buf.length === 29

      val streamRead = new ByteArrayInputStream(buf)
      val reader = new BinaryReader(streamRead)

      reader.readBinary() === Array(Byte.MinValue, Byte.MaxValue)
      reader.readBinary() === Array.empty[Byte]
      reader.readBinary() === Array(10.toByte, 11.toByte, 12.toByte)
    }

    "read and write UUID" in {
      val streamWrite = new ByteArrayOutputStream()
      val writer = new BinaryWriter(streamWrite)

      val uuid1 = UUID.randomUUID()
      val uuid2 = UUID.randomUUID()
      val uuid3 = UUID.randomUUID()

      writer.writeUUID(uuid1)
      writer.writeUUID(uuid2)
      writer.writeUUID(uuid3)
      writer.close()

      val buf = streamWrite.toByteArray()
      buf.length === 48

      val streamRead = new ByteArrayInputStream(buf)
      val reader = new BinaryReader(streamRead)

      reader.readUUID() === uuid1
      reader.readUUID() === uuid2
      reader.readUUID() === uuid3
    }

    "read and write Date" in {
      val streamWrite = new ByteArrayOutputStream()
      val writer = new BinaryWriter(streamWrite)

      val date1 = new Date(53274832L)
      val date2 = new Date(12353274832L)
      val date3 = new Date()

      writer.writeDate(date1)
      writer.writeDate(date2)
      writer.writeDate(date3)
      writer.close()

      val buf = streamWrite.toByteArray()
      buf.length === 24

      val streamRead = new ByteArrayInputStream(buf)
      val reader = new BinaryReader(streamRead)

      reader.readDate() === date1
      reader.readDate() === date2
      reader.readDate() === date3
    }
  }
}