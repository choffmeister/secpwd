package de.choffmeister.secpwd.utils

import org.specs2.mutable._
import org.junit.runner.RunWith
import org.specs2.runner.JUnitRunner
import de.choffmeister.secpwd.utils.RichStream._
import java.io.ByteArrayInputStream
import java.io.ByteArrayOutputStream
import java.io.IOException
import java.io.EOFException

@RunWith(classOf[JUnitRunner])
class RichStreamSpec extends Specification {
  "RichInputStream" should {
    "cap inner streams" in {
      val buf = Array[Byte](0, 1, 2, 3, 4)
      val ms = new ByteArrayInputStream(buf)
      ms.read() === 0.toByte
      ms.read() === 1.toByte

      ms.preSizedInner(2) { s =>
        s.read() === 2.toByte
        s.read() === 3.toByte
        s.read() === -1
      }

      ms.read() === 4.toByte
    }

    "ensure proper stream position" in {
      val buf = Array[Byte](0, 1, 2, 3, 4)
      val ms = new ByteArrayInputStream(buf)
      ms.read() === 0.toByte
      ms.read() === 1.toByte

      ms.preSizedInner(2) { s =>
        s.read() === 2.toByte
      }

      ms.read() === 4.toByte
    }

    "fail if presized inner so to large" in {
      val buf = Array[Byte](0, 1, 2, 3, 4)
      val ms = new ByteArrayInputStream(buf)
      ms.read() === 0.toByte
      ms.read() === 1.toByte

      ms.preSizedInner(10) { s =>
        s.read() === 2.toByte
      } must throwA[EOFException]

      ok
    }

    "ensure non negative length" in {
      val ms = new ByteArrayInputStream(Array.empty)
      ms.preSizedInner(-1L)(_.read()) must throwA[IndexOutOfBoundsException]
    }
  }

  "RichOutputStream" should {
    "cap inner streams" in {
      val ms = new ByteArrayOutputStream()
      ms.write(0)
      ms.write(1)

      ms.preSizedInner(3) { s =>
        s.write(2)
        s.write(3)
        s.write(4)
        s.write(5)
      } must throwA[IOException]

      ms.write(6)

      ms.toByteArray.toList === List[Byte](0, 1, 2, 3, 4, 6)
    }

    "ensure proper stream position" in {
      val ms = new ByteArrayOutputStream()
      ms.write(0)
      ms.write(1)

      ms.preSizedInner(3) { s =>
        s.write(2)
        s.write(3)
      }

      ms.toByteArray.toList === List[Byte](0, 1, 2, 3, 0)
    }

    "cache output streams" in {
      val ms = new ByteArrayOutputStream()
      ms.write(0)
      ms.write(1)

      ms.cached(cs => ms.write(cs.size)) { cs =>
        cs.write(10)
        cs.write(11)
        cs.write(12)
        cs.write(13)
        
        ms.size === 2
      }

      ms.write(14)

      ms.toByteArray.toList === List[Byte](0, 1, 4, 10, 11, 12, 13, 14)
    }

    "ensure non negative length" in {
      val ms = new ByteArrayOutputStream()
      ms.preSizedInner(-1L)(_.write(0)) must throwA[IndexOutOfBoundsException]
    }
  }
}