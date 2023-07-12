// Main.scala --- (c) 2021, 2023 John Maraist
// Part of the WTUL Roster Generator
//
// This file is made available under the GNU GPL version 3; see the
// LICENSE file in this distribution or https://fsf.org/ for more
// details.

package wtul.rosters
import java.time.LocalDate
import org.maraist.wtulrosters.{
  PsaLongTermSpots, PsaRosters, PsaScheduling,
  PromoLongTermSpots, PromoRosters, PromoScheduling,
  Alerts, Voice, Inventory,
  PsaShortTermSpots, PromoShortTermSpots}
import org.maraist.wtulrosters.Utils.syncRosters

@main def batch: Unit = {
  PromoRosters.init()
  val promoOut = PromoRosters.writeNWeeks()

  PsaRosters.init()
  val psaOut = PsaRosters.writeNWeeks()

  syncRosters(
    promoOut.result() ++ psaOut.result(),
    List("promos-weeks.xml", "psa-weeks.xml")
  )
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

@main def inventory: Unit =
  Inventory.writeInventory(
    PsaLongTermSpots, PsaShortTermSpots,
    PromoLongTermSpots, PromoShortTermSpots)

@main def alerts: Unit = {
  Alerts.printAlerts()
}

@main def voicetest: Unit = {
  Voice.translateTestSSML3
  // Voice.translateTestSSML4
  // Voice.listVoices
}
