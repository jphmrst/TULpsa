// Exporter.scala --- (c) 2021 John Maraist
// Part of the WTUL Roster Generator
//
// This file is made available under the GNU GPL version 3; see the
// LICENSE file in this distribution or https://fsf.org/ for more
// details.

package org.maraist.wtulrosters
import java.io.{File, FileWriter, BufferedWriter}
import java.time.LocalDateTime
import scala.collection.mutable.Builder

/** Exporter for rosters as XML.
  */
class Exporter(val rosters: Set[Roster]) {
  /** TODO
    */
  def write(filename: String = "export"): Unit = {
    val bw = Exporter.openXml(filename)
    Exporter.toXml(bw, this)
    Exporter.closeXml(bw)
  }
}

object Exporter {
  /** TODO
    */
  def openXml(rootFile: String = "export"): BufferedWriter = {
    val bw = new BufferedWriter(new FileWriter(new File(rootFile + ".xml")))

    bw.write("<?xml version=\"1.0\" encoding=\"UTF-8\" ?>\n")
    bw
  }

  /** TODO
    */
  def closeXml(doc: BufferedWriter) = {
    doc.close
  }

  /** TODO
    */
  def toXml(bw: BufferedWriter, exporter: Exporter): Unit = {
    bw.write("<tul:roster xmlns=\"http://www.w3.org/1999/xhtml\"\n")
    bw.write("        xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"\n")
    bw.write("        xmlns:tul=\"https://maraist.net/wtul/rosters\"\n")
    bw.write("        xsi:schemaLocation=\"https://maraist.net/wtul/rosters rosterexport.xsd\"\n")

    bw.write(s"        generationDate=\"${LocalDateTime.now()}\"\n")

    bw.write(" >\n")

    val spots = {
      val buf = Set.newBuilder[Spot]
      for (roster <- exporter.rosters; spot <- roster.slots) do buf += spot
      buf.result
    }

    for (spot <- spots) do {
      bw.write(s"  <tul:psa id=\"${spot.tag}\">\n")
      bw.write(s"    ${spot.text.toHTML}\n")
      bw.write("  </tul:psa>\n")
    }

    for (
      roster <- exporter.rosters;
      dayIdx <- 0 until roster.rosterDays;
      daySlotIdx <- 0 until roster.hourSlots(dayIdx).size
    ) do {
      val spotIdx = roster.hourSlots(dayIdx)(daySlotIdx)
      val dateTime = roster.slotDateTime(dayIdx, daySlotIdx)
      val spot = roster.slots(spotIdx)
      bw.write(s"  <tul:reading slot=\"${dateTime}:00\" id=\"${spot.tag}\" />\n")
    }
    // ???

    bw.write("</tul:roster>\n")
  }
}
