
package org.maraist.wtulrosters

import scala.collection.mutable.HashSet

/** Representation of the group associated with announcements
  * ([[Spot]]s).
  * @param tag A short [[String]] associated with the group.
  * @param title The fill title of the group.
  * @param period The period (in weeks) of the rise and fall of the
  * priority for announcements associated with this group.
  * @param boost The relative boost (upward compression) of the
  * priority curve for announcements associated with this group.
  *
  * This trait is sealed so that the instances defined in the
  * companion object are the only possible instances.
  */
sealed trait Group(
  val tag: String,
  val title: String,
  val period: Double = Group.DEFAULT_PERIOD,
  val boost: Double = 0.0
) {
  if boost >= 1.0 || boost < 0.0 then
    throw new IllegalArgumentException
      ("boost must be at least 0.0 and below 1.0")

  Group.inventory.add(this)
}

/** The available [[Group]] instances, and standardized periods for
  * priority cycling.
  */
object Group {
  /**
    * The default period for the cycle of a PSA spot's priority for
    * long-term scheduling.
    * @group periods
    */
  val DEFAULT_PERIOD: Double = 8.0

  /**
    * The suggested period for the cycle of a PSA spot's priority for
    * long-term scheduling of seasonal spots.
    * @group periods
    */
  val SEASONAL_PERIOD: Double = DEFAULT_PERIOD / 2.0

  /**
    * The suggested period for the cycle of a PSA spot's priority for
    * long-term scheduling of less-frequent spots.
    * @group periods
    */
  val LONG_PERIOD: Double = 2.0 * DEFAULT_PERIOD

  /**
    * The suggested period for the cycle of a PSA spot's priority for
    * long-term scheduling of rare spots.
    * @group periods
    */
  val RARE_PERIOD: Double = 3.0 * DEFAULT_PERIOD

  /** Return the list of all known [[Group]]s */
  def all = List.from(inventory)
  private val inventory = new HashSet[Group]

  /** Group for volunteer solicitations.
    *  @group groups
    */
  object Volunteer extends Group("Volunteer", "Volunteer opportunities")

  /** Group for educational opportunity announcements.
    *  @group groups
    */
  object Edu extends Group("Edu", "Educational opportunities")

  /** Group for announcements of public services.
    *  @group groups
    */
  object Services extends Group("Services", "Public services")

  /** Group for ecological/environmental announcements.
    *  @group groups
    */
  object Eco extends Group("Eco", "Environmental and ecological")

  /** Group for public health announcements.
    *  @group groups
    */
  object Health extends Group("Health", "Health services")

  /** Group for mental health announcements.
    *  @group groups
    */
  object Mental extends Group("Mental", "Mental health services")

  /** Group for announcements about civic matters.
    *  @group groups
    */
  object Civic extends Group("Civic", "Civic opportunities and notices")

  /** Group for animal care announcements.
    *  @group groups
    */
  object Animal extends Group("Animal", "Animal and pet care")

  /** Group for local museum announcements.
    *  @group groups
    */
  object Museum extends Group("Museum", "Local museums", LONG_PERIOD)

  /** Group for announcements which are aired only rarely.
    *  @group groups
    */
  object Rare extends Group("Rare", "Rare", RARE_PERIOD)

  /** Group for tax-related announcements which are not specific to the
    * annual tax season.
    *  @group groups
    */
  object TaxAlways extends Group("TaxAlways", "Off-season tax", LONG_PERIOD)

  /** Group for announcements related to the annual tax season.
    *  @group groups
    */
  object Taxtime extends Group("Taxtime", "Tax season",
    SEASONAL_PERIOD,
    boost = 0.6)

  /** Group for announcements relevant to hurricane season.
    *  @group groups
    */
  object StormPrep extends Group("StormPrep", "Hurricane preparation",
    SEASONAL_PERIOD,
    boost = 0.7)

  /** Group for announcements relevant to the winter holiday season.
    *  @group groups
    */
  object Holiday extends Group("Holiday", "Winter holiday seasonal",
    SEASONAL_PERIOD,
    boost = 0.6)

  /** Group for announcements relevant in Carnival season.
    *  @group groups
    */
  object Carnival extends Group("Carnival", "Carnival season",
    SEASONAL_PERIOD,
    boost = 0.7)

  /** Group for announcements relevant in summer.
    *  @group groups
    */
  object Summer extends Group("Summer", "Summer season",
    SEASONAL_PERIOD,
    boost = 0.6)

  /** Group for announcements about voting, for the times before
    * elections.
    *  @group groups
    */
  object Voter extends Group("Voter", "Voter information", boost = 0.2)
}
