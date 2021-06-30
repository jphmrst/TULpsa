package org.maraist.wtulrosters

import java.time.LocalDate
import org.maraist.latex.LaTeXdoc
import java.time.Period

@main def testRun: Unit = {
  import Spots.stringToLocalDate
  import scala.language.implicitConversions

  Spots.init()
  Assortment.init()

  print("Writing report...")
  writeReport()
  println("finished")

  val startDate = LocalDate.parse("2021-07-05")
  for (i <- 0 until 6) {
    val thisDate = startDate.plusDays(7 * i)
    print(s"Creating roster for $thisDate...")
    val builder = new PsaRosterBuilder(thisDate)
    builder.completeWith(Assortment.getSortedList(thisDate))
    builder.result().write()
    println("written")
  }
}

def restRun: Unit = {
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
  val startDate = LocalDate.parse("2022-01-15")
  val doc = new LaTeXdoc("report")
  doc.addPackage("times")
  doc.addPackage("geometry", "margin=0.5in")
  doc.addPackage("supertabular")
  doc.addPackage("colortbl")
  doc.addPackage("xcolor")

  doc.open()

  doc ++=/ "\\section{Groups}"

  doc ++=/ """\begin{supertabular}{|cc|}"""
  doc ++=/ """ \multicolumn{1}{c}{Tag} & \multicolumn{1}{c}{Period} \\ \hline"""
  for(group <- Group.all) {
    doc ++=/ s" \\textsc{\\small ${group.tag}} & ${group.period.toString()} \\\\ \\hline"
  }
  doc ++=/ """\end{supertabular}"""

  doc ++=/ "\\section{Spots}"

  doc ++=/ """\begin{supertabular}{|l|cccc|rr|}"""
  doc ++=/ """ \multicolumn{1}{l}{Tag}"""
  doc ++=/ """  & Group"""
  doc ++=/ """  & Start"""
  doc ++=/ """  & End"""
  doc ++=/ """  & \multicolumn{1}{c}{Gain\,$\times$}"""
  doc ++=/ """  & \multicolumn{1}{c}{Wiggle}"""
  doc ++=/ """  & \multicolumn{1}{c}{Period}"""
  doc ++=/ """  \\ \hline"""
  for(spot <- Spot.all) {
    doc ++=/ s" \\textsc{\\small ${spot.tag}}"
    doc ++=/ s"  & \\textsc{\\small ${spot.group.tag}}"
    doc ++=/ s"  & ${spot.start.toString()}"
    doc ++=/ s"  & ${spot.end.map(_.toString()).getOrElse("-")}"
    doc ++=/ s"  & ${spot.groupGainMultiplier.toString()}"
    doc ++=/ f"  & ${spot.periodWiggle}%.3f"
    doc ++=/ f"  & ${spot.period}%.3f"
    doc ++=/ s"  \\\\ \\hline"
  }
  doc ++=/ """\end{supertabular}"""

  doc ++=/ "\\section{Assortments}"

  doc ++=/ "\\subsection{Gain}"

  doc ++=/ """\begin{supertabular}{|l|cccccccccccc|}"""
  doc ++=/ """  \multicolumn{1}{l}{Group tag}"""
  doc ++=/ """  & Jan"""
  doc ++=/ """  & Feb"""
  doc ++=/ """  & Mar"""
  doc ++=/ """  & Apr"""
  doc ++=/ """  & May"""
  doc ++=/ """  & Jun"""
  doc ++=/ """  & Jul"""
  doc ++=/ """  & Aug"""
  doc ++=/ """  & Sep"""
  doc ++=/ """  & Oct"""
  doc ++=/ """  & Nov"""
  doc ++=/ """  & \multicolumn{1}{c}{Dec}"""
  doc ++=/ """  \\ \hline"""
  for(group <- Group.all) {
    doc ++=/ s" \\textsc{\\small ${group.tag}} "
    for(i <- 0 until 12) {
      val hereDate = startDate.plusMonths(i)
      val assortment = Assortment(hereDate)
      doc ++=/ s"  & ${assortment.groups.get(group).map(_.toString()).getOrElse("-")}"
    }
    doc ++=/ """  \\ \hline"""
  }
  doc ++=/ """\end{supertabular}"""

  doc ++=/ "\\subsection{Spot priority}"

  doc ++=/ "\\small{"
  doc ++=/ """\begin{supertabular}{|l@{~}c@{~}c@{~}c@{~}c@{~}c@{~}c@{~}c@{~}c@{~}c@{~}c@{~}c@{~}c@{~}c@{~}c@{~}c@{~}c@{~}c@{~}c@{~}c@{~}c@{~}c@{~}c@{~}c@{~}c@{~}|}"""
  doc ++=/ """  \multicolumn{1}{c}{}"""
  doc ++=/ """  & \multicolumn{2}{c}{Jan}"""
  doc ++=/ """  & \multicolumn{2}{c}{Feb}"""
  doc ++=/ """  & \multicolumn{2}{c}{Mar}"""
  doc ++=/ """  & \multicolumn{2}{c}{Apr}"""
  doc ++=/ """  & \multicolumn{2}{c}{May}"""
  doc ++=/ """  & \multicolumn{2}{c}{Jun}"""
  doc ++=/ """  & \multicolumn{2}{c}{Jul}"""
  doc ++=/ """  & \multicolumn{2}{c}{Aug}"""
  doc ++=/ """  & \multicolumn{2}{c}{Sep}"""
  doc ++=/ """  & \multicolumn{2}{c}{Oct}"""
  doc ++=/ """  & \multicolumn{2}{c}{Nov}"""
  doc ++=/ """  & \multicolumn{2}{c}{Dec}"""
  doc ++=/ """  \\ """
  doc ++=/ """  \multicolumn{1}{l}{}"""
  for(i <- 0 until 12) { doc ++=/ """  & Gn & \multicolumn{1}{l}{Pr} """ }
  doc ++=/ """  \\ \hline"""
  for(spot <- Spot.all) {
    doc ++=/ s" \\multicolumn{25}{|l|}{${spot.tag} (${spot.group.tag})}"
    doc ++=/ """  \\"""
    for(i <- 0 until 12) {
      val hereDate = startDate.plusMonths(i)
      val assortment = Assortment(hereDate)
      doc ++=/ s"  & \\textcolor{black!80}{${assortment.groups.get(spot.group).map("%.2f".format(_)).getOrElse("-")}}"
      doc ++=/ f"    & \textbf{${assortment.spotPriority(spot, hereDate)}%.2f}"
    }
    doc ++=/ """  \\ \hline"""
  }
  doc ++=/ """\end{supertabular}"""
  doc ++=/ "}"

  doc ++=/ """\subsection{Sorted pairs list (top 20)}"""
  for(i <- 0 until 20) {
    val thisDate = startDate.plusDays(7 * i)
    doc ++=/ s"\\subsubsection{${thisDate.toString()}}"
    doc ++=/ """\begin{supertabular}{rcrrr}"""
    doc ++=/ """ \multicolumn{1}{c}{Spot}"""
    doc ++=/ """  & Group"""
    doc ++=/ """  & \multicolumn{1}{c}{Base}"""
    doc ++=/ """  & \multicolumn{1}{c}{Gain}"""
    doc ++=/ """  & \multicolumn{1}{c}{Priority}"""
    doc ++=/ """  \\ \hline"""
    var lastBase = 1.0
    for(pair <- Assortment.getSortedPairsList(thisDate).take(20)) pair match {
      case (spot, priority) => {
        doc ++=/ f" ${spot.tag}%s"
        doc ++=/ f" & ${spot.group.tag}%s"
        val thisBase = spot.priority(thisDate)
        doc ++=/ f" & ${if thisBase > lastBase then "\\cellcolor{red!70}" else ""}${thisBase}%.9f"
        lastBase = thisBase
        doc ++=/ s" & ${Assortment(thisDate).groups.get(spot.group).map("%.4f".format(_)).getOrElse("-")}"
        doc ++=/ f" & ${priority}%.9f"
        doc ++=/ """ \\"""
      }
    }
    doc ++=/ """ \hline"""
    doc ++=/ """\end{supertabular}"""
  }

  doc.close()
}

