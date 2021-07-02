// Type.scala --- (c) 2021 John Maraist
// Part of the WTUL Roster Generator
//
// This file is made available under the GNU GPL version 3; see the
// LICENSE file in this distribution or https://fsf.org/ for more
// details.

package org.maraist.wtulrosters
import java.time.LocalDate
import scala.collection.mutable.Builder

/** Top-level abstraction of constructing one type of rosters.  The
  * sole public API operations are for initialization, and for
  * constructing the roster for a particular day.  The representation
  * of the [[RosterBuilder]], and the steps for creating it and then
  * the [[Roster]], are all internal and must be provided by concrete
  * subclasses.
  */
abstract class RosterType {

  /** We expect most concrete implementations to be given as Scala
    * `object`s.  Since Scala instantiates objects lazily, this method
    * provides explicit initialization.
    */
  def init(): Unit

  /** Generate a roster anchored on the given `date`.
    * @return The root name of the generated file.
    */
  def writeFor(date: LocalDate = LocalDate.now()): String = {
    val anchorDate = singleAnchorDate(date)
    val builder = newBuilder(anchorDate)
    complete(builder)
    val roster = builder.result()
    roster.write()
    roster.fileTitle
  }

  /** Write several weeks of rosters beginning from a particular date.
    * @return The [[Builder]] for the [[Seq]] of [[String]]s naming
    * the generated files.
    */
  def writeNWeeks(
    date: LocalDate = LocalDate.now(),
    outBuilder: Builder[String, ? <: Seq[String]] = Seq.newBuilder,
    n: Int = 6
  ): Builder[String, ? <: Seq[String]] = {
    val anchorDate = singleAnchorDate(date)
    for (i <- 0 until n) {
      val thisDate = anchorDate.plusDays(7 * i)
      val fileroot = writeFor(thisDate)
      outBuilder += fileroot + ".pdf"
    }
    outBuilder
  }

  /** Given an arbitrary date, return the date which should actually be
    * used when generating a single roster to cover the given date.
    * The default implementation returns a Monday as-is, or otherwise
    * the first Monday before the given `date`.
    */
  protected def singleAnchorDate(date: LocalDate): LocalDate = {
    import java.time.DayOfWeek.*
    date.getDayOfWeek match {
      case MONDAY => date
      case TUESDAY => date.minusDays(1)
      case WEDNESDAY => date.minusDays(2)
      case THURSDAY => date.minusDays(3)
      case FRIDAY => date.minusDays(4)
      case SATURDAY => date.minusDays(5)
      case SUNDAY => date.minusDays(6)
    }
  }

  /** Given an arbitrary date, return the date which should actually be
    * used when generating a range of rosters covering the given date.
    * The default implementation returns the first Monday before the
    * given `date`, going back a week when a Monday is given as the
    * argument.
    */
  protected def rangeAnchorDate(date: LocalDate): LocalDate = {
    import java.time.DayOfWeek.*
    date.getDayOfWeek match {
      case MONDAY => date.minusWeeks(1)
      case TUESDAY => date.minusDays(1)
      case WEDNESDAY => date.minusDays(2)
      case THURSDAY => date.minusDays(3)
      case FRIDAY => date.minusDays(4)
      case SATURDAY => date.minusDays(5)
      case SUNDAY => date.minusDays(6)
    }
  }

  /** Internal implementation of the [[RosterBuilder]] used in this
    * class.
    */
  protected type RBuilder <: RosterBuilder

  /** Factory method for the internal implementation of the
    * [[RosterBuilder]] used here.
    */
  protected def newBuilder(date: LocalDate): RBuilder

  /** This method should populate the internal implementation of the
    * [[RosterBuilder]] with the contents for the given anchor date.
    */
  protected def complete(builder: RBuilder): Unit
}
