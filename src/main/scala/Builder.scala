
package org.maraist.wtulrosters
import java.time.{LocalDate,LocalDateTime}
import java.time.format.DateTimeFormatter

class RosterBuilder(
  val startDate: LocalDate,
  val size: Int,
  val title: String,
  val groupLead: String,
  val footer: String,
  val indexFormatter: Int => String,
  val preamble: String,
  val timestamper: DateTimeFormatter,
  val filePrefix: String,
  val blockPolicy: (Int, Int) => Int
) {

  val slots: Array[Unassigned | Spot] =
    Array.fill[Unassigned | Spot](size)(Unassigned.ITEM)

  def result(): Roster = new Roster(
    startDate,
    Array.tabulate[Spot](size)((i) => slots(i) match {
      case _: Unassigned =>
        throw new IllegalStateException(s"Slot $i unassigned")
      case s: Spot => s
    }),
    title, groupLead, footer, indexFormatter, preamble, timestamper
  ) {
    override def fileTitle: String = filePrefix + super.fileTitle
  }

  def apply(i: Int): Unassigned | Spot = slots(i)

  def set(i: Int, spot: Spot): Unit = { slots(i) = spot }

  def unassignedRanges: List[(Int, Int)] = findNextRange(0)

  private def findNextRange(from: Int): List[(Int, Int)] =
    if from < size
    then slots(from) match {
      case _: Unassigned => completeRange(from, 1 + from)
      case spot: Spot => findNextRange(1 + from)
    }
    else Nil

  private def completeRange(start: Int, next: Int): List[(Int, Int)] =
    if next < size
    then slots(next) match {
      case _: Unassigned => completeRange(start, 1 + next)
      case spot: Spot =>
        (start, next - 1) :: findNextRange(1 + next)
    }
    else List((start, next - 1))

  def completeWith(spots: List[Spot]): Unit =
    completeWith(unassignedRanges, spots)

  private def completeWith(ranges: List[(Int, Int)], spots: List[Spot]):
      Unit = {
    // println(s"completeWith($ranges)")
    ranges match {
      case Nil => { }
      case (first, last) :: rest => completeWith(first, last, rest, spots)
    }
  }

  private def completeWith
    (first: Int, last: Int, ranges: List[(Int, Int)], spots: List[Spot]):
      Unit = {
    // println(s"completeWith($first, $last, $ranges)")
    spots match {
      case Nil => throw new IllegalArgumentException("Exhausted spots list")
      case spot :: otherSpots => {
        val len = blockPolicy(first, last)
        // println(s"  policy($first, $last) = $len")
        for(i <- 0 until len) {
        // println(s"  ${i + first} <- ${spot.tag}")
          set(i + first, spot)
        }
        if first + len > last
        then completeWith(ranges, otherSpots)
        else completeWith(first + len, last, ranges, otherSpots)
      }
    }
  }
}
