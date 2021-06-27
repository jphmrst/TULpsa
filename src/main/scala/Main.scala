package org.maraist.wtulrosters

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
}
