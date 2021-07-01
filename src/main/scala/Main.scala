// Main.scala --- (c) 2021 John Maraist
// Part of the WTUL Roster Generator
//
// This file is made available under the GNU GPL version 3; see the
// LICENSE file in this distribution or https://fsf.org/ for more
// details.

package wtul.rosters

import java.time.LocalDate
import org.maraist.latex.LaTeXdoc
import java.time.Period
import java.time.DayOfWeek.*
import scala.sys.process.*
import scala.collection.mutable.Builder
import org.maraist.wtulrosters.
  {Group, Spot, PsaSpots, Assortment, PsaRosters, RosterType, SpotBank}
import org.maraist.wtulrosters.writeInternalReport

@main def batch: Unit = {
  initializeStatics
  val startDate = pastMonday
  val out = Seq.newBuilder[String]
  writeNWeeks(startDate, PsaRosters, out)
  syncRosters(out.result())
}

@main def testRun: Unit = {
  initializeStatics
  print("Writing report...")
  writeInternalReport(PsaSpots)
  println("finished")
  writeNWeeks(LocalDate.parse("2021-06-28"), PsaRosters)
}

def initializeStatics: Unit = {
  PsaSpots.init()
  Assortment.init()
}

def pastMonday: LocalDate = {
  val today = LocalDate.now
  today.getDayOfWeek match {
    case MONDAY => today.minusWeeks(1)
    case TUESDAY => today.minusDays(1)
    case WEDNESDAY => today.minusDays(2)
    case THURSDAY => today.minusDays(3)
    case FRIDAY => today.minusDays(4)
    case SATURDAY => today.minusDays(5)
    case SUNDAY => today.minusDays(6)
  }
}

def writeNWeeks(
  date: LocalDate,
  rosterType: RosterType,
  outBuilder: Builder[String, ? <: Seq[String]] = Seq.newBuilder,
  n: Int = 6
) = {
  for (i <- 0 until n) {
    val thisDate = date.plusDays(7 * i)
    rosterType.writeFor(thisDate)
    outBuilder += s"PSA-${thisDate}.pdf"
  }
}

def syncRosters(files: Seq[String]) = {
  import scala.util.Properties.{envOrNone, envOrElse}
  envOrNone("WTUL_ROSTERS_UPLOAD") match {
    case None => {}
    case Some(hostPath) => {
      import scala.language.postfixOps
      print(s"Uploading to ${hostPath}...")
      val options = envOrElse("WTUL_ROSTERS_RSYNC_OPTS", "")
      (s"rsync --delete-excluded --recursive $options ${files.fold("")(_ + " " + _)} $hostPath" !)
      println("written")
    }
  }
}
