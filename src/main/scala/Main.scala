package org.maraist.wtulrosters

@main def hello: Unit =
  Spots.init()
  println()
  println(Spot.size.toString() + " spots")
  println()
