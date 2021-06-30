
package org.maraist.wtulrosters
import java.time.LocalDate

/** One PSA spot
  */
class Spot(
  val tag: String,
  val group: Group,
  val text: String,
  val copresent: Option[String] = None,
  val start: LocalDate,
  val alert: LocalDate,
  val end: Option[LocalDate] = None,
  val sourceContacts: Seq[String] = Seq(),
  val sourceURL: Seq[String] = Seq(),
  val sourceNote: Option[String] = None,
  val groupGainMultiplier: Double = 1.0,
  val variantGroup: Int = Spot.nextVariantGroup,
  val boost: Double = 0.0
)(using bank: SpotBank) {
  import Spot.{getWeekOfCentury, EPSILON}

  // -----------------------------------------------------------------
  // Instance creation.

  // Correctness checks --- make sure each spot has a unique tag.
  if (bank.tags.contains(tag))
    then throw new IllegalArgumentException("Duplicate tag " + tag)

  if boost >= 1.0 || boost < 0.0 then
    throw new IllegalArgumentException
      ("boost must be at least 0.0 and below 1.0")

  // Register this instance in the various Spot tables.
  bank.inventory += this
  bank.tags += ((tag, this))
  bank.grouped.put(group, this :: bank.grouped.getOrElse(group, Nil))

  // End of instance creation.
  // -----------------------------------------------------------------

  /** We will use the hash code of the text several times, so store it
    * here.
    */
  private val testHash: Int = text.hashCode()

  /** Calculate a small, unique, persistent "wiggle" for the period of
    * every spot, so that it has at least a slightly unique pattern.
    */
  private[wtulrosters]
  val periodWiggle: Double = Math.sin(testHash.toDouble) / 4.0

  /** The period (in weeks) of the rise and fall of this spot's priority
    * ranking.
    */
  val period: Double = group.period + periodWiggle

  /** Calculate the spot's priority on a given date.
    */
  def priority(date: LocalDate): Double = {
    val curvePoint =
      Math.sin(date.getWeekOfCentury().toDouble / period * 2.0 * Math.PI)
    val inUnit = curvePoint / (2.0 - 2 * EPSILON) + 0.5 + EPSILON
    val withSpotBoost = (inUnit * (1 - boost)) + boost
    val withGroupBoost = (withSpotBoost * (1 - group.boost)) + group.boost
    withGroupBoost
  }

  /** Assemble the introductory text (excluding the "optional" or
    * "required" prefix) for this spot.
    */
  def introText: String = {
    "This public service announcement is brought to you by "
    + copresent.map(_ + " and ").getOrElse("")
    + "WTUL New Orleans."
  }
}

object Spot {
  /** Calculate the week number of a date with its century, under the
    * simplifying assumptions that each year has exactly 52 weeks, and
    * January 1 always starts a new week.
    */
  extension (localDate: LocalDate)
      def getWeekOfCentury(): Int =
        (localDate.getYear() % 100) * 52 + localDate.getDayOfYear() / 7

  /** A very small number, used to return non-zero priorities.
    */
  val EPSILON: Double = 0.0000001

  private var nextVariantCounter: Int = 1
  def nextVariantGroup: Int = {
    val result = nextVariantCounter
    nextVariantCounter = nextVariantCounter + 1
    result
  }
}

class SpotBank {
  given stringToLocalDate: Conversion[String, LocalDate] = LocalDate.parse(_)
  given optionPresent[A]: Conversion[A, Option[A]] with
      def apply(a: A): Option[A] = Some(a)
  given singletonSeq[A]: Conversion[A, Seq[A]] with
      def apply(a: A): Seq[A] = Seq(a)
  given SpotBank = this

  private[wtulrosters] val inventory =
    new scala.collection.mutable.HashSet[Spot]
  private[wtulrosters] val tags =
    new scala.collection.mutable.HashMap[String,Spot]
  private[wtulrosters] val grouped =
    new scala.collection.mutable.HashMap[Group,List[Spot]]

  def all: Iterable[Spot] = inventory
  def size: Int = inventory.size
  def ofGroup(g: Group): List[Spot] = grouped.getOrElse(g, Nil)

  def init(): Unit = { }

  def getSortedPairsList(date: LocalDate): List[(Spot, Double)] = {
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
      // print("\n\t*** " + groupGain.toString() + "  " + group.title)
      for (spot <- ofGroup(group)) {
        if spot.start.compareTo(date) <= 0
           && spot.end.map(date.compareTo(_) <= 0).getOrElse(true)
        then {
          val basePriority = spot.priority(date)
          val spotGain = groupGain * spot.groupGainMultiplier / 10.0
          val finalPriority = Math.pow(basePriority, Math.exp(- spotGain))
          acc += ((spot, finalPriority))
          // printf("\n  %s\t%f\t%f\t%f", spot.tag, basePriority, spotGain, finalPriority)
        }
      }
    }
    // println()

    List.from(acc.result())
  }

  def getSortedList(date: LocalDate): List[Spot] =
    getSortedPairsList(date).map({ case (s, _) => s})
}
