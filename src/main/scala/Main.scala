// Main.scala --- (c) 2021 John Maraist
// Part of the WTUL Roster Generator
//
// This file is made available under the GNU GPL version 3; see the
// LICENSE file in this distribution or https://fsf.org/ for more
// details.

package wtul.rosters
import org.maraist.wtulrosters.{PsaSpots, PsaRosters}
import org.maraist.wtulrosters.writeInternalReport
import org.maraist.wtulrosters.Utils.syncRosters

@main def batch: Unit = {
  PsaRosters.init()
  val out = PsaRosters.writeNWeeks()
  syncRosters(out.result())
}

@main def diagnostic: Unit = {
  PsaRosters.init()
  print("Writing report...")
  writeInternalReport(PsaSpots)
  println("finished")
  PsaRosters.writeNWeeks()
}
