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
  val end: LocalDate = LocalDate.MAX
)(using schedule: AssortmentSchedule) {

  schedule.assortmentSet.add(this)

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
  given Ordering[Assortment] with
      def compare(a: Assortment, b: Assortment): Int =
        b.start compareTo (a.start)

  val always = (date: LocalDate) => true

  val HIGH_HIGH_GAIN = 25.0
  val HIGH_GAIN = 18.0
  val MED_HIGH_GAIN = 8.0
  val MED_GAIN = 6.0
  val MED_LOW_GAIN = 5.0
  val LOW_GAIN = 3.0
  val NO_GAIN = 0.0
  val NEG_GAIN = -2.0

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
}

class AssortmentSchedule {
  given AssortmentSchedule = this

  def init() = { }

  val assortmentSet = new TreeSet[Assortment]

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

  def apply(when: LocalDate): Assortment = returning {
    for (assortment <- assortmentSet) {
      if (assortment.start compareTo when) <= 0 then
      if (when compareTo (assortment.end)) <= 0 then
      if assortment.predicate(when) then throwReturn(assortment)
    }

    throw new IllegalArgumentException(s"No assortment available for $when")
  }
}
