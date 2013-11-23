package de.choffmeister.secpwd.utils

import java.io.File
import java.io.PrintWriter
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
}

object RichFile {
  implicit def fileToRichFile(file: File) = new RichFile(file)
}