// Main.scala --- (c) 2021 John Maraist
// Part of the WTUL Roster Generator
//
// This file is made available under the GNU GPL version 3; see the
// LICENSE file in this distribution or https://fsf.org/ for more
// details.

package wtul.rosters
import java.time.LocalDate
import org.maraist.wtulrosters.{
  PsaLongTermSpots, PsaRosters, PsaScheduling, Alerts, Voice}
import org.maraist.wtulrosters.Utils.syncRosters

@main def batch: Unit = {
  PsaRosters.init()
  val out = PsaRosters.writeNWeeks()
  syncRosters(out.result())
}

@main def diagnostic: Unit = {
  PsaRosters.init()
  print("Writing report...")
  PsaLongTermSpots.writeInternalReport(PsaScheduling)
  println("finished")
  PsaRosters.writeNWeeks()
}

@main def one: Unit = {
  PsaRosters.init()
  PsaRosters.writeFor(LocalDate.parse("2021-07-01"))
  PsaRosters.writeFor(LocalDate.parse("2021-07-07"))
  PsaRosters.writeFor(LocalDate.parse("2021-07-14"))
  PsaRosters.writeFor(LocalDate.parse("2021-07-21"))
}

@main def alerts: Unit = {
  Alerts.printAlerts()
}

@main def voicetest: Unit = {
  Voice.translateTestSSML3
  // Voice.listVoices
}
