package org.maraist.wtulrosters

import java.time.LocalDate

@main def testRun: Unit = {
  Spots.init()
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
}
