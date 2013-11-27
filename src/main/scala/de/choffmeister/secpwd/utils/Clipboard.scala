package de.choffmeister.secpwd.utils

import java.awt.datatransfer.StringSelection
import java.awt.Toolkit

object Clipboard {
  def put(str: String) {
    val sel = new StringSelection(str)
    val clipboard = Toolkit.getDefaultToolkit().getSystemClipboard()
    clipboard.setContents(sel, sel)
  }
}
