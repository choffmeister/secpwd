package de.choffmeister.secpwd.utils

import java.io.{File, PrintWriter, FileInputStream, FileOutputStream}
import scala.io.Source
import scala.io.Codec
import scala.language.implicitConversions

class RichFile(val file: File) {
  def text: String = Source.fromFile( file )(Codec.UTF8).mkString

  def text_=(s: String) {
    val out = new PrintWriter(file, "UTF-8")
    try {
      out.print(s)
     }
    finally {
      out.close()
    }
  }

  def bytes: Array[Byte] = {
    val fs = new FileInputStream(file)
    try {
      val length = file.length().toInt
      val b = new Array[Byte](length)
      fs.read(b, 0, length)
      b
    } finally {
      fs.close()
    }
  }

  def bytes_=(b: Array[Byte]) {
    val fs = new FileOutputStream(file)
    try {
      fs.write(b, 0, b.length)
    } finally {
      fs.close()
    }
  }
}

object RichFile {
  implicit def fileToRichFile(file: File) = new RichFile(file)
}