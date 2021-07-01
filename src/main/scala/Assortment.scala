// Assortment.scala --- (c) 2021 John Maraist
// Part of the WTUL Roster Generator
//
// This file is made available under the GNU GPL version 3; see the
// LICENSE file in this distribution or https://fsf.org/ for more
// details.

package org.maraist.wtulrosters
import java.time.LocalDate
import scala.collection.mutable.TreeSet
import scala.math.Ordering
import scala.util.control.NonLocalReturns.*

/** Specification of the groups and associated spot gains for a given
  * period of time.
  */
case class Assortment(
  val start: LocalDate,
  val predicate: LocalDate => Boolean,
  val groups: Map[Group, Double],
  val end: LocalDate
) {
  Assortment.assortmentSet.add(this)

  def spotPriority(spot: Spot, date: LocalDate): Double = {
    val basePriority = spot.priority(date)
    groups.get(spot.group) match {
      case None => 0.0
      case Some(groupGain) => {
        val spotGain = groupGain * spot.groupGainMultiplier
        Math.pow(basePriority, Math.exp(- spotGain))
      }
    }
  }
}

object Assortment {
  import Group.*

  def init() = { }

  def apply(
    start: String,
    predicate: LocalDate => Boolean,
    groups: Map[Group, Double]
  ): Assortment =
    new Assortment(LocalDate.parse(start), predicate, groups, LocalDate.MAX)

  def apply(
    start: String,
    predicate: LocalDate => Boolean,
    groups: Map[Group, Double],
    end: LocalDate
  ): Assortment =
    new Assortment(LocalDate.parse(start), predicate, groups, end)

  given Ordering[Assortment] with
      def compare(a: Assortment, b: Assortment): Int =
        b.start compareTo (a.start)

  val assortmentSet = new TreeSet[Assortment]

  val always = (date: LocalDate) => true

  val HIGH_HIGH_GAIN = 25.0
  val HIGH_GAIN = 18.0
  val MED_HIGH_GAIN = 8.0
  val MED_GAIN = 6.0
  val MED_LOW_GAIN = 5.0
  val LOW_GAIN = 3.0
  val NO_GAIN = 0.0
  val NEG_GAIN = -2.0

  // Default assortment when nothing else applies
  private val baseWeights = Map(
    Volunteer -> MED_LOW_GAIN,  Health -> MED_LOW_GAIN,
    Mental -> LOW_GAIN, Services -> LOW_GAIN,
    Voter -> NO_GAIN, Civic -> NO_GAIN,
    Eco -> NO_GAIN,    Animal -> NO_GAIN, Edu -> NO_GAIN,
    TaxAlways -> NO_GAIN, Museum -> NO_GAIN,
    Rare -> NEG_GAIN
  )

  private val DEFAULT_ASSORTMENT =
    Assortment(LocalDate.MIN, always, baseWeights,
    LocalDate.MAX)

  import java.time.Month.*

  val isJanuary = (date: LocalDate) => date.getMonth() == JANUARY
  val isFebruary = (date: LocalDate) => date.getMonth() == FEBRUARY
  val isMarch = (date: LocalDate) => date.getMonth() == MARCH
  val isApril = (date: LocalDate) => date.getMonth() == APRIL
  val isMay = (date: LocalDate) => date.getMonth() == MAY
  val isJune = (date: LocalDate) => date.getMonth() == JUNE
  val isJuly = (date: LocalDate) => date.getMonth() == JULY
  val isAugust = (date: LocalDate) => date.getMonth() == AUGUST
  val isSeptember = (date: LocalDate) => date.getMonth() == SEPTEMBER
  val isOctober = (date: LocalDate) => date.getMonth() == OCTOBER
  val isNovember = (date: LocalDate) => date.getMonth() == NOVEMBER
  val isDecember = (date: LocalDate) => date.getMonth() == DECEMBER

  Assortment("2021-01-01", isJanuary,
    baseWeights + (Taxtime -> MED_HIGH_GAIN) - TaxAlways + (Carnival -> HIGH_HIGH_GAIN))
  Assortment("2021-02-01", isFebruary,
    baseWeights + (Taxtime -> HIGH_GAIN) - TaxAlways)
  Assortment("2021-03-01", isMarch,
    baseWeights + (Taxtime -> MED_HIGH_GAIN) + (Voter -> HIGH_GAIN) - TaxAlways)
  Assortment("2021-04-01", isApril,
    baseWeights + (Taxtime -> MED_HIGH_GAIN) + (Voter -> HIGH_GAIN) - TaxAlways)
  Assortment("2021-05-01", isMay, baseWeights + (Summer -> HIGH_GAIN))
  Assortment("2021-06-01", isJune,
    baseWeights + (Summer -> HIGH_GAIN) + (StormPrep -> HIGH_HIGH_GAIN))
  Assortment("2021-07-01", isJuly,
    baseWeights + (Summer -> MED_HIGH_GAIN) + (StormPrep -> HIGH_GAIN))
  Assortment("2021-08-01", isAugust,
    baseWeights + (StormPrep -> MED_HIGH_GAIN) + (Voter -> MED_HIGH_GAIN))
  Assortment("2021-09-01", isSeptember, baseWeights + (Voter -> HIGH_GAIN))
  Assortment("2021-10-01", isOctober, baseWeights + (Voter -> HIGH_GAIN))
  Assortment("2021-11-01", isNovember, baseWeights)
  Assortment("2021-12-01", isDecember,
    baseWeights - Services + (Services -> HIGH_HIGH_GAIN)
      - Health + (Health -> HIGH_HIGH_GAIN)
      - Mental + (Mental -> HIGH_HIGH_GAIN)
      + (Holiday -> HIGH_HIGH_GAIN))

  def apply(when: LocalDate): Assortment = returning {
    for (assortment <- assortmentSet) {
      if (assortment.start compareTo when) <= 0 then
      if (when compareTo (assortment.end)) <= 0 then
      if assortment.predicate(when) then throwReturn(assortment)
    }

    DEFAULT_ASSORTMENT
  }
}
