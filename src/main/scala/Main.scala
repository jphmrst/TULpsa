package org.maraist.wtulrosters

import java.time.LocalDate
import org.maraist.latex.LaTeXdoc

@main def testRun: Unit = {
  Spots.init()
  Assortment.init()

  print("Writing report...")
  writeReport()
  println("finished")

  println()
  println(Spot.size.toString() + " spots")

  import Spots.{stringToLocalDate, voteDotOrg, hnoc}
  import scala.language.implicitConversions
  val roster = new PsaRoster(
    "2021-06-26",
    Array(
      voteDotOrg, voteDotOrg,
      hnoc, hnoc,
      voteDotOrg, voteDotOrg, voteDotOrg,
      hnoc, hnoc, hnoc,
      voteDotOrg, voteDotOrg, voteDotOrg, voteDotOrg,
      hnoc, hnoc, hnoc, hnoc,
      voteDotOrg, voteDotOrg, voteDotOrg, voteDotOrg, voteDotOrg,
      hnoc, hnoc, hnoc, hnoc, hnoc,
      voteDotOrg, voteDotOrg, voteDotOrg, voteDotOrg, voteDotOrg, voteDotOrg,
      hnoc, hnoc, hnoc, hnoc, hnoc, hnoc,
      voteDotOrg, voteDotOrg, voteDotOrg, voteDotOrg, voteDotOrg, voteDotOrg, voteDotOrg,
      hnoc, hnoc, hnoc, hnoc, hnoc, hnoc, hnoc,
      voteDotOrg, voteDotOrg, voteDotOrg, voteDotOrg, voteDotOrg, voteDotOrg, voteDotOrg, voteDotOrg,
      hnoc, hnoc, hnoc, hnoc, hnoc, hnoc, hnoc, hnoc,
      voteDotOrg, voteDotOrg, voteDotOrg, voteDotOrg, voteDotOrg, voteDotOrg, voteDotOrg, voteDotOrg,
    )
  )
  println("Created roster")

  roster.write()
  println("Written")

  def priorityln(spot: Spot, start: LocalDate) = {
    for(i <- 0 to 10) {
      val when = start.plusDays(7 * i)
      println(
        spot.tag + " " + when.toString + ": " + spot.priority(when).toString())
    }
  }

  priorityln(hnoc, "2021-06-26")
  println()

  def topSpots(date: LocalDate) = {
    print(date.toString + ": ")
    for (spot <- Assortment.getSortedList(date).take(12)) {
      print(" " + spot.tag)
    }
    println()
  }
  topSpots("2021-06-05")
  topSpots("2021-06-12")
  topSpots("2021-06-19")
  topSpots("2021-06-26")
  topSpots("2021-07-05")
  topSpots("2021-07-12")
  topSpots("2021-07-19")
  topSpots("2021-07-26")

  println(Assortment.assortmentSet.size.toString())
}

def writeReport() = {
  val doc = new LaTeXdoc("report")
  doc.addPackage("times")
  doc.addPackage("supertabular")

  doc.open()

  doc ++=/ """\begin{supertabular}{lll|}"""
  doc ++=/ """ Tag & Start & End \\ \hline"""
  for(spot <- Spot.all) {
    doc ++=/ s" ${spot.tag} & ${spot.start.toString()} & ${spot.end.map(_.toString()).getOrElse("-")} \\\\ \\hline"
  }
  doc ++=/ """\end{supertabular}"""

  doc.close()
}

