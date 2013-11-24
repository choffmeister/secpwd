package de.choffmeister.secpwd.utils

import java.io.File
import java.io.IOException
import java.io.EOFException
import java.io.InputStream
import java.io.OutputStream
import java.io.ByteArrayOutputStream
import java.security.MessageDigest
import java.security.DigestInputStream
import java.security.DigestOutputStream
import scala.language.implicitConversions

class RichInputStream(val stream: InputStream) {
  def preSizedInner(size: Long)(inner: InputStream => Any) {
    val wrapper = new PreSizedInnerInputStream(size, stream)
    try {
      inner(wrapper)
    } finally {
      wrapper.close()
    }
  }

  /**
   * Optimize (for example implement read(Array[Byte], Int, Int) => Int)
   */
  class PreSizedInnerInputStream(val size: Long, val inner: InputStream) extends InputStream {
    private var position = 0L

    override def read(): Int = {
      if (position < size) {
        val b = inner.read()
        if (b >= 0) {
          position += 1
          b
        } else -1
      } else -1
    }

    override def close(): Unit = {
      while (position < size) {
        val b = read()
        if (b < 0) throw new EOFException()
      }
      super.close()
    }
  }
}

class RichOutputStream(val stream: OutputStream) {
  def preSizedInner(size: Long)(inner: OutputStream => Any) {
    val wrapper = new PreSizedInnerOutputStream(size, stream)
    try {
      inner(wrapper)
    } finally {
      wrapper.close()
    }
  } 

  def cached(after: ByteArrayOutputStream => Any)(inner: ByteArrayOutputStream => Any) {
    val cache = new ByteArrayOutputStream()
    inner(cache)
    after(cache)
    val buf = cache.toByteArray
    stream.write(buf)
  }

  /**
   * Optimize (for example implement write(Array[Byte], Int, Int) => Unit)
   */
  class PreSizedInnerOutputStream(val size: Long, val inner: OutputStream) extends OutputStream {
    private var position = 0L

    override def write(b: Int): Unit = {
      if (position < size) {
        position += 1
        inner.write(b)
      } else throw new IOException()
    }

    override def close(): Unit = {
      while (position < size) write(0)
      super.close()
    }
  }
}

object RichStream {
  implicit def inputStreamToRichInputStream(stream: InputStream) = new RichInputStream(stream)
  implicit def outputStreamToRichOutputStream(stream: OutputStream) = new RichOutputStream(stream)
}