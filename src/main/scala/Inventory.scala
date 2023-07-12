// Inventory.scala --- (c) 2023 John Maraist
// Part of the WTUL Roster Generator
//
// This file is made available under the GNU GPL version 3; see the
// LICENSE file in this distribution or https://fsf.org/ for more
// details.

package org.maraist.wtulrosters
import java.time.{LocalDate, LocalDateTime, LocalTime}
import java.time.format.DateTimeFormatter
import org.maraist.structext.{SpeakAs, StructText, fromString}
import org.maraist.structext.StructText.*
import scala.collection.mutable.HashMap
import org.maraist.latex.{LaTeXRenderable,LaTeXdoc}

object Inventory {

  def writeInventory(
    longPsas: SpotBank, shortPsas: SpotBank,
    longPromos: SpotBank, shortPromos: SpotBank): Unit = {

    val doc = new LaTeXdoc("inventory")
    doc.setClass("article")
    doc.addPackage("geometry", "margin=1in")
    doc.addPackage("paralist")
    doc.title = "WTUL PSA/Promo Current Inventory"
    doc.author = "The All-Knowing Announcement Automaton"
    doc.date = "Generated \\today"

    doc.open()
    doc ++= "\\setcounter{tocdepth}{1} "
    doc ++= "\\maketitle "
    doc ++= "\\tableofcontents "

    longPromos.writeInventory(doc, "Long-term promos")
    shortPromos.writeInventory(doc, "Short-term promos")
    longPsas.writeInventory(doc, "Long-term PSAs")
    shortPsas.writeInventory(doc, "Short-term PSAs")
    doc.close()
  }

}

