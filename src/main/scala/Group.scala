
package org.maraist.wtulrosters

import scala.collection.mutable.HashSet

/**
  * The default period for the cycle of a PSA spot's priority for
  * long-term scheduling.
  */
val DEFAULT_PERIOD: Double = 8.0

/**
  * The suggested period for the cycle of a PSA spot's priority for
  * long-term scheduling of seasonal spots.
  */
val SEASONAL_PERIOD: Double = DEFAULT_PERIOD / 2.0

/**
  * The suggested period for the cycle of a PSA spot's priority for
  * long-term scheduling of less-frequent spots.
  */
val LONG_PERIOD: Double = 2.0 * DEFAULT_PERIOD

/**
  * The suggested period for the cycle of a PSA spot's priority for
  * long-term scheduling of rare spots.
  */
val RARE_PERIOD: Double = 3.0 * DEFAULT_PERIOD

sealed trait Group(
  val tag: String,
  val title: String,
  val period: Double = DEFAULT_PERIOD,
  val boost: Double = 0.0
) {
  if boost >= 1.0 || boost < 0.0 then
    throw new IllegalArgumentException
      ("boost must be at least 0.0 and below 1.0")

  Group.inventory.add(this)
}

object Group {
  private val inventory = new HashSet[Group]
  def all = List.from(inventory)

  object Volunteer extends Group("Volunteer", "Volunteer opportunities")
  object Edu extends Group("Edu", "Educational opportunities")
  object Services extends Group("Services", "Public services")
  object Eco extends Group("Eco", "Environmental and ecological")
  object Health extends Group("Health", "Health services")
  object Mental extends Group("Mental", "Mental health services")
  object Civic extends Group("Civic", "Civic opportunities and notices")
  object Animal extends Group("Animal", "Animal and pet care")
  object Museum extends Group("Museum", "Local museums", LONG_PERIOD)
  object Rare extends Group("Rare", "Rare", RARE_PERIOD)
  object TaxAlways extends Group("TaxAlways", "Off-season tax", LONG_PERIOD)

  object Taxtime extends Group("Taxtime", "Tax season",
    SEASONAL_PERIOD,
    boost = 0.6)
  object StormPrep extends Group("StormPrep", "Hurricane preparation",
    SEASONAL_PERIOD,
    boost = 0.7)
  object Holiday extends Group("Holiday", "Winter holiday seasonal",
    SEASONAL_PERIOD,
    boost = 0.6)
  object Carnival extends Group("Carnival", "Carnival season",
    SEASONAL_PERIOD,
    boost = 0.7)
  object Summer extends Group("Summer", "Summer season",
    SEASONAL_PERIOD,
    boost = 0.6)
  object Voter extends Group("Voter", "Voter information", boost = 0.2)
}
