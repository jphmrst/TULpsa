
package org.maraist.wtulrosters
import java.time.LocalDate

/** Objects of this class are designed to hold several [[Spot]]s.
  */
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
