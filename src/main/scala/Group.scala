
package org.maraist.wtulrosters

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

sealed trait Group(val title: String, val period: Double = DEFAULT_PERIOD)

object Group {
  object Volunteer extends Group("Volunteer opportunities")
  object Edu extends Group("Educational opportunities")
  object Services extends Group("Public services")
  object Eco extends Group("Environmental and ecological")
  object Health extends Group("Health services")
  object Mental extends Group("Mental health services")
  object Civic extends Group("Civic opportunities and notices")
  object Animal extends Group("Animal and pet care")
  object Museum extends Group("Local museums", LONG_PERIOD)
  object Rare extends Group("Rare", RARE_PERIOD)
  object Taxtime extends Group("Tax season", SEASONAL_PERIOD)
  object TaxAlways extends Group("Off-season tax", LONG_PERIOD)
  object StormPrep extends Group("Hurricane preparation", SEASONAL_PERIOD)
  object Holiday extends Group("Winter holiday seasonal", SEASONAL_PERIOD)
  object Carnival extends Group("Carnival season", SEASONAL_PERIOD)
  object Summer extends Group("Summer season", SEASONAL_PERIOD)
  object Voter extends Group("Voter information")
}

