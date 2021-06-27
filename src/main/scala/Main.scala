package org.maraist.wtulrosters

@main def hello: Unit = {
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

  import org.maraist.latex.LaTeXdoc
  val doc = new LaTeXdoc("2021-06-20")
  doc.open()
  println("Opened output")

  roster.toLaTeX(doc)
  println("Written")

  println("Closed\n")
}
