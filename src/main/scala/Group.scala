
package org.maraist.wtulrosters

sealed trait Group(val title: String)

object Group {
  object Volunteer extends Group("Volunteer opportunities")
  object Edu extends Group("Educational opportunities")
  object Services extends Group("Public services")
  object Eco extends Group("Environmental and ecological")
  object Health extends Group("Health services")
  object Mental extends Group("Mental health services")
  object Civic extends Group("Civic opportunities and notices")
  object Animal extends Group("Animal and pet care")
  object Museum extends Group("Local museums")
  object Rare extends Group("Rare announcements")
  object Taxtime extends Group("Tax season announcements")
  object TaxAlways extends Group("Off-season tax announcements")
  object StormPrep extends Group("Hurricane preparation")
  object Holiday extends Group("Holidays")
  object Carnival extends Group("Carnival seasonal announcements")
  object Summer extends Group("Summer seasonal announcements")
  object Voter extends Group("Voter information announcements")
}

