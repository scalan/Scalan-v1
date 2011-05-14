package scalan.util

import scala.text._
import Document._

object ArrayFormatting {
  val ED: scala.text.Document = empty

  def format(doc: Document): String = {
    val writer = new java.io.StringWriter()
    doc.format(120, writer)
    writer.toString
  }
}
