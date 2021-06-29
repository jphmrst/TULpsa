
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

  val HIGH_GAIN = 0.6
  val MED_GAIN = 0.4
  val MED_HIGH_GAIN = (HIGH_GAIN + MED_GAIN) / 2.0
  val LOW_GAIN = 0.1
  val MED_LOW_GAIN = (LOW_GAIN + MED_GAIN) / 2.0
  val NO_GAIN = 0.0
  val NEG_GAIN = -0.3

  // Default assortment when nothing else applies
  private val DEFAULT_ASSORTMENT =
    Assortment(LocalDate.MIN, always, Map(
      Volunteer -> NO_GAIN, Edu -> NO_GAIN, Services -> NO_GAIN, Eco -> NO_GAIN,
      Health -> NO_GAIN, Mental -> NO_GAIN, Civic -> NO_GAIN, Animal -> NO_GAIN,
      Museum -> NO_GAIN, Rare -> NEG_GAIN
    ),
    LocalDate.MAX)

  private val baseWeights = Map(
    Volunteer -> MED_HIGH_GAIN, Health -> MED_HIGH_GAIN,
    Mental -> MED_GAIN,    Civic -> MED_GAIN,  Services -> MED_GAIN,
    Voter -> MED_LOW_GAIN, Edu -> MED_LOW_GAIN,
    Eco -> MED_LOW_GAIN,   Animal -> MED_LOW_GAIN,
    TaxAlways -> LOW_GAIN,
    Rare -> NO_GAIN
  )

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
    baseWeights + (Taxtime -> MED_HIGH_GAIN) - TaxAlways + (Carnival -> MED_HIGH_GAIN))
  Assortment("2021-02-01", isFebruary,
    baseWeights + (Taxtime -> MED_HIGH_GAIN) - TaxAlways)
  Assortment("2021-03-01", isMarch,
    baseWeights + (Taxtime -> MED_HIGH_GAIN) + (Voter -> MED_HIGH_GAIN) - TaxAlways)
  Assortment("2021-04-01", isApril,
    baseWeights + (Taxtime -> MED_HIGH_GAIN) + (Voter -> MED_HIGH_GAIN) - TaxAlways)
  Assortment("2021-05-01", isMay, baseWeights + (Summer -> MED_LOW_GAIN))
  Assortment("2021-06-01", isJune,
    baseWeights + (Summer -> MED_LOW_GAIN) + (StormPrep -> HIGH_GAIN))
  Assortment("2021-07-01", isJuly,
    baseWeights + (Summer -> LOW_GAIN) + (StormPrep -> MED_LOW_GAIN))
  Assortment("2021-08-01", isAugust,
    baseWeights + (StormPrep -> MED_LOW_GAIN) + (Voter -> MED_GAIN))
  Assortment("2021-09-01", isSeptember, baseWeights + (Voter -> MED_HIGH_GAIN))
  Assortment("2021-10-01", isOctober, baseWeights + (Voter -> MED_GAIN))
  Assortment("2021-11-01", isNovember, baseWeights)
  Assortment("2021-12-01", isDecember,
    baseWeights - Services + (Services -> MED_HIGH_GAIN)
      - Health + (Health -> MED_HIGH_GAIN)
      - Mental + (Mental -> MED_HIGH_GAIN)
      + (Holiday -> HIGH_GAIN))

  def apply(when: LocalDate): Assortment = returning {
    for (assortment <- assortmentSet) {
      if (assortment.start compareTo when) <= 0 then
      if (when compareTo (assortment.end)) <= 0 then
      if assortment.predicate(when) then throwReturn(assortment)
    }

    DEFAULT_ASSORTMENT
  }

  def getSortedList(date: LocalDate): List[Spot] = {
    val acc = scala.collection.mutable.SortedSet.newBuilder[(Spot, Double)](
      new Ordering[(Spot, Double)] {
        def compare(p1: (Spot, Double), p2: (Spot, Double)) = p1 match {
          case (_, d1) => p2 match {
            case (_, d2) => d2 compare d1
          }
        }
      }
    )

    for ((group, groupGain) <- Assortment(date).groups) {
      print("\n\t*** " + groupGain.toString() + "  " + group.title)
      for (spot <- Spot.ofGroup(group)) {
        val basePriority = spot.priority(date)
        val spotGain = groupGain * spot.groupGainMultiplier / 10.0
        val finalPriority = Math.pow(basePriority, Math.exp(- spotGain))
        acc += ((spot, finalPriority))
        printf("\n  %s\t%f\t%f\t%f",
          spot.tag, basePriority, spotGain, finalPriority)
      }
    }
    println()

    List.from(acc.result()).map({ case (s, _) => s})
  }
}
