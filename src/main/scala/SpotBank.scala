// SpotBank.scala --- (c) 2021 John Maraist
// Part of the WTUL Roster Generator
//
// This file is made available under the GNU GPL version 3; see the
// LICENSE file in this distribution or https://fsf.org/ for more
// details.

package org.maraist.wtulrosters
import java.time.LocalDate
import org.maraist.latex.LaTeXdoc
import org.maraist.wtulrosters.Utils.{twoPlaces, fourPlaces}

/** Objects of this class are designed to hold several [[Spot]]s.
  */
class SpotBank(val tag: String, val schedule: AssortmentSchedule)
    extends Utils.Converters {

  /** Provide a way to register new [[Spot]]s within the scope of
    * instances and subclass of this class.
    */
  protected given addSpot: ((Spot) => Unit) = (spot) => {
    // Fail early if the new Spot's tag is already in use.
    if (tags.contains(spot.tag))
      then throw new IllegalArgumentException("Duplicate tag " + spot.tag)

    // Register the new tag.
    tags += ((spot.tag, spot))

    // Register the spot.
    inventory += spot

    // Add the new spot to its group's manifest. */
    grouped.put(spot.group, spot :: grouped.getOrElse(spot.group, Nil))
  }

  /** Storage for all of the [[Spot]]s associated with this bank.
    */
  private val inventory = new scala.collection.mutable.HashSet[Spot]

  /** Storing all of the [[Spot]]s associated with this bank for lookup
    * by their tag.
    */
  private val tags = new scala.collection.mutable.HashMap[String,Spot]

  /** Storing all of the [[Spot]]s associated with this bank for lookup
    * by their [[Group]].
    */
  private val grouped = new scala.collection.mutable.HashMap[Group,List[Spot]]

  /** Returns the [[Spot]]s associated with this bank. */
  def all: Iterable[Spot] = inventory

  /** Returns the number of [[Spot]]s associated with this bank. */
  def size: Int = inventory.size

  /** Returns the [[Spot]]s from this bank which are part of the given
    * [[Group]].
    */
  def ofGroup(g: Group): List[Spot] = grouped.getOrElse(g, Nil)

  /** Calling this method forces the initialization of [[Spot]]s
    * included in banks defined by `object`.
    */
  def init(): Unit = { }

  def writeInternalReport(schedule: AssortmentSchedule) = {
    val startDate = LocalDate.parse("2022-01-15")
    val doc = new LaTeXdoc(s"${tag}-bank-report")
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
    for(spot <- all) {
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
        val assortment: Assortment = schedule(hereDate)
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
    for(spot <- all) {
      val spotGroup: Group = spot.group
      doc ++=/ s" \\multicolumn{25}{|l|}{${spot.tag} (${spot.group.tag})}"
      doc ++=/ """  \\"""
      for(i <- 0 until 12) {
        val hereDate = startDate.plusMonths(i)
        val assortment = schedule(hereDate)
        val gain: Option[Double] = assortment.groups.get(spotGroup)
        doc ++=/ s"  & \\textcolor{black!80}{${gain.map(twoPlaces(_)).getOrElse("-")}}"
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
      for(pair <- schedule.getSortedPairsList(thisDate, this).take(20)) pair match {
        case (spot, priority) => {
          doc ++=/ f" ${spot.tag}%s"
          doc ++=/ f" & ${spot.group.tag}%s"
          val thisBase = spot.priority(thisDate)
          doc ++=/ f" & ${if thisBase > lastBase then "\\cellcolor{red!70}" else ""}${thisBase}%.9f"
          lastBase = thisBase
          doc ++=/ s" & ${schedule(thisDate).groups.get(spot.group).map(fourPlaces(_)).getOrElse("-")}"
          doc ++=/ f" & ${priority}%.9f"
          doc ++=/ """ \\"""
        }
      }
      doc ++=/ """ \hline"""
      doc ++=/ """\end{supertabular}"""
    }

    doc.close()
  }
}
