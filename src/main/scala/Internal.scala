
package org.maraist.wtulrosters
import java.time.LocalDate
import org.maraist.latex.LaTeXdoc

def writeInternalReport() = {
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
