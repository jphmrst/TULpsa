
package org.maraist.wtulrosters
import java.time.{LocalDate,LocalDateTime}
import java.time.format.DateTimeFormatter

/** Builders for [[Roster]] instances.
  *
  * This class loosely follows the standard Builder pattern
  * manifestation in Scala, in that the final artifact is obtained
  * with a call to `result()`.  However there is no `+=` method, and
  * this class does not extend the
  * [[scala.collection.mutable.Builder]] trait: the expected use of
  * these builders will require a higher level of inspection of
  * progress than what this trait supports.
  *
  * @param startDate The starting date for the [[Roster]] to be
  * constructed.
  * @param title The title of the resulting document.
  * @param groupLead The text prepended to the index/indices of
  * announcements when the result is output.
  * @param footer Text to be included at the bottom of every page of this
  * result's output.
  * @param indexFormatter Formatter from indices into `slots` to their
  * representation in the result's output.
  * @param preamble Declarations to be included in the result's output's LaTeX
  * preamble.
  * @param timestamper Renderer for the timestamp in the result's
  * output.
  * @param filePrefix String to be prepended to the formatted date
  * when the result is output.
  * @param blockPolicy Describes the number of consecutive slots to be
  * occupied by a [[Spot]] drawn from a long-term or other default
  * bank.
  */
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

  /** Internal storage for the slots of the [[Roster]] we are building.
    */
  private val slots: Array[Unassigned | Spot] =
    Array.fill[Unassigned | Spot](size)(Unassigned.ITEM)

  /** Build and return a [[Roster]].
    *
    * This method should not be called unless a [[Spot]] has been
    * assigned to all `size` slots.  Assembling the result will fail
    * with an [[IllegalStateException]] if an [[Unassigned]] slot is
    * found.
    */
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

  /** Returns the [[Spot]] assigned at the given index, or the
    * [[Unassigned.ITEM unassigned]] placeholder if none is assigned
    * yet.
    */
  def apply(i: Int): Unassigned | Spot = slots(i)

  /** Assign the `spot` to the given index `i`.  */
  def set(i: Int, spot: Spot): Unit = { slots(i) = spot }

  /** Return a list of `(first, last)` pairs of the intervals of
    * consecutive [[Unassigned.ITEM unassigned]] slots in the builder.
    * Note that the `last` element of each pair is the index of the
    * actual last element, '''not''' the element past the last
    * element.
    */
  def unassignedRanges: List[(Int, Int)] = findNextRange(0)

  /** Recursive helper method for [[#unassignedRanges]]. */
  private def findNextRange(from: Int): List[(Int, Int)] =
    if from < size
    then slots(from) match {
      case _: Unassigned => completeRange(from, 1 + from)
      case spot: Spot => findNextRange(1 + from)
    }
    else Nil

  /** Recursive helper method for [[#unassignedRanges]]. */
  private def completeRange(start: Int, next: Int): List[(Int, Int)] =
    if next < size
    then slots(next) match {
      case _: Unassigned => completeRange(start, 1 + next)
      case spot: Spot =>
        (start, next - 1) :: findNextRange(1 + next)
    }
    else List((start, next - 1))

  /** Fill in unassigned slots with [[Spot]]s drawn (in order) from the
    * given list.
    */
  def completeWith(spots: List[Spot]): Unit =
    completeWith(unassignedRanges, spots)

  /** Recursive helper method for [[#completeWith]]. */
  private def completeWith(ranges: List[(Int, Int)], spots: List[Spot]):
      Unit = {
    // println(s"completeWith($ranges)")
    ranges match {
      case Nil => { }
      case (first, last) :: rest => completeWith(first, last, rest, spots)
    }
  }

  /** Recursive helper method for [[#completeWith]]. */
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
