// SpotBank.scala --- (c) 2021 John Maraist
// Part of the WTUL Roster Generator
//
// This file is made available under the GNU GPL version 3; see the
// LICENSE file in this distribution or https://fsf.org/ for more
// details.

package org.maraist.wtulrosters
import java.time.LocalDate

/** Objects of this class are designed to hold several [[Spot]]s.
  */
class SpotBank {

  /** Convert a [[String]] to a [[LocalDate]] by parsing the string.
    */
  given stringToLocalDate: Conversion[String, LocalDate] = LocalDate.parse(_)

  /** Convert any [[A]] instance to an [[Option]][A] instance by tagging
    * it [[Some]].
    */
  given optionPresent[A]: Conversion[A, Option[A]] with
      def apply(a: A): Option[A] = Some(a)

  /** Convert any [[A]] instance to an [[Seq]][A] instance by making it
    * a singleton sequence.
    */
  given singletonSeq[A]: Conversion[A, Seq[A]] with
      def apply(a: A): Seq[A] = Seq(a)

  /** Provide a way to register new [[Spot]]s within the scope of
    * instances and subclass of this class.
    */
  protected given addSpot: ((Spot) => Unit) = (spot) => {
    // Fail early if the new Spot's tag is already in use.
    if (tags.contains(spot.tag))
      then throw new IllegalArgumentException("Duplicate tag " + spot.tag)

    // Register the new tag.
    tags += ((spot.tag, spot))

    // Register the spot.
    inventory += spot

    // Add the new spot to its group's manifest. */
    grouped.put(spot.group, spot :: grouped.getOrElse(spot.group, Nil))
  }

  /** Storage for all of the [[Spot]]s associated with this bank.
    */
  private val inventory = new scala.collection.mutable.HashSet[Spot]

  /** Storing all of the [[Spot]]s associated with this bank for lookup
    * by their tag.
    */
  private val tags = new scala.collection.mutable.HashMap[String,Spot]

  /** Storing all of the [[Spot]]s associated with this bank for lookup
    * by their [[Group]].
    */
  private val grouped = new scala.collection.mutable.HashMap[Group,List[Spot]]

  /** Returns the [[Spot]]s associated with this bank. */
  def all: Iterable[Spot] = inventory

  /** Returns the number of [[Spot]]s associated with this bank. */
  def size: Int = inventory.size

  /** Returns the [[Spot]]s from this bank which are part of the given
    * [[Group]].
    */
  def ofGroup(g: Group): List[Spot] = grouped.getOrElse(g, Nil)

  /** Calling this method forces the initialization of [[Spot]]s
    * included in banks defined by `object`.
    */
  def init(): Unit = { }

  /** Returns a list of [[Spot]]-priority pairs drawn from this bank,
    * ordered by priority.
    */
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

  /** Returns a list of [[Spot]]s drawn from this bank, ordered by their
    * priority for the given date.
    */
  def getSortedList(date: LocalDate): List[Spot] =
    getSortedPairsList(date).map({ case (s, _) => s})
}
