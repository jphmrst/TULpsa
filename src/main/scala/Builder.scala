// Builder.scala --- (c) 2021 John Maraist
// Part of the WTUL Roster Generator
//
// This file is made available under the GNU GPL version 3; see the
// LICENSE file in this distribution or https://fsf.org/ for more
// details.

package org.maraist.wtulrosters

import java.time.{LocalDate,LocalDateTime}
import java.time.format.DateTimeFormatter
import scala.util.control.NonLocalReturns.*

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
abstract class RosterBuilder(
  val species: String,
  val startDate: LocalDate,
  val size: Int,
  val mixer: AssortmentSchedule,
  val title: String,
  val groupLead: String,
  val footer: String,
  val indexFormatter: Int => String,
  val preamble: String,
  val timestamper: DateTimeFormatter,
  val filePrefix: String,
  val blockPolicy: (Int, Int) => Int,
  val slotDays: Array[Int | List[Int]],
  val hourSlots: Array[Array[Int]],
  val useOptIntro: Boolean,
  val tagTextColor: String
) {

  if (slotDays.length != size)
    throw new IllegalArgumentException(
      s"$size slots, but slotDays array has length ${slotDays.length}")

  /** Internal storage for the slots of the [[Roster]] we are building.
    */
  private val slots: Array[Unassigned | Spot] =
    Array.fill[Unassigned | Spot](size)(Unassigned.ITEM)

  def rosterSlotDateTime(roster: Roster, day: Int, idx: Int): LocalDateTime
  def rosterDayCount: Int
  def rosterDaySlotCount(dayIdx: Int): Int

  /** Build and return a [[Roster]].
    *
    * This method should not be called unless a [[Spot]] has been
    * assigned to all `size` slots.  Assembling the result will fail
    * with an
    * [[java.lang.IllegalStateException][IllegalStateException]] if an
    * [[Unassigned]] slot is found.
    */
  def result(): Roster = new Roster(
    species,
    startDate,
    Array.tabulate[Spot](size)((i) => slots(i) match {
      case _: Unassigned =>
        throw new IllegalStateException(s"Slot $i unassigned")
      case s: Spot => s
    }),
    title, groupLead, footer, indexFormatter, preamble, timestamper, hourSlots,
    useOptIntro, tagTextColor
  ) {
    override def fileTitle: String = filePrefix + super.fileTitle
    override def slotDateTime(dayIdx: Int, idx: Int): LocalDateTime =
      rosterSlotDateTime(this, dayIdx, idx)
    override def rosterDays: Int = rosterDayCount
    override def rosterDaySlots(dayIdx: Int): Int = rosterDaySlotCount(dayIdx)
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

  /** Return the pair of assigned neighbors of a certain slot, if any.
    */
  def assignedNeighbors(slot: Int, within: Int = size):
      (Option[Spot], Option[Spot]) = {
    (
      returning {
        for (i <- 0 to within; if slot - i >= 0)
          slots(slot - i) match {
            case _: Unassigned => { }
            case spot: Spot => throwReturn[Option[Spot]](Some(spot))
          }
        None
      },
      returning {
        for (i <- 0 to within; if slot + i < size)
          slots(slot + i) match {
            case _: Unassigned => { }
            case spot: Spot => throwReturn[Option[Spot]](Some(spot))
          }
        None
      }
    )
  }

  def nearSameGroup(slot: Int, candidate: Spot, within: Int = 3): Boolean = {
    val group = candidate.variantGroup
    val (lower, upper): (Option[Spot], Option[Spot]) =
      assignedNeighbors(slot, within)
    val checker: Spot => Boolean =
      (spot) => !(spot == candidate) && spot.variantGroup == group
    lower.map(checker).getOrElse(false) || upper.map(checker).getOrElse(false)
  }

  /** Fill by day matching. */
  def fillByDayMatch(bank: SpotBank): Unit = {
    Output.fullln(s"\nfillByDayMatch with ${bank.tag}")

    // Pull the slots available on each day covered by this roster.
    val dailyInventories = Array.tabulate[Set[Spot]](7)(
      (i: Int) => Set.from(bank.getSortedList(startDate.plusDays(i))))

    Output.fullln("- dailyInventory[0] = " +
      List.from(dailyInventories(0)).map(_.tag.toString()).fold("")(_ + _))
    Output.fullln("- dailyInventory[1] = " +
      List.from(dailyInventories(1)).map(_.tag.toString()).fold("")(_ + _))
    Output.fullln("- dailyInventory[2] = " +
      List.from(dailyInventories(2)).map(_.tag.toString()).fold("")(_ + _))
    Output.fullln("- dailyInventory[3] = " +
      List.from(dailyInventories(3)).map(_.tag.toString()).fold("")(_ + _))
    Output.fullln("- dailyInventory[4] = " +
      List.from(dailyInventories(4)).map(_.tag.toString()).fold("")(_ + _))
    Output.fullln("- dailyInventory[5] = " +
      List.from(dailyInventories(5)).map(_.tag.toString()).fold("")(_ + _))
    Output.fullln("- dailyInventory[6] = " +
      List.from(dailyInventories(6)).map(_.tag.toString()).fold("")(_ + _))

    // Prioritize placing another instance of the spot we just
    // placed.
    var lastSpot: Spot | Unassigned = Unassigned.ITEM
    var lastSlot: Int = size

    // Now look at each slot in this roster.
    for (rosterSlot <- slotOrder) {
      Output.full(s"- Checking slot $rosterSlot...")

      // If that slot is already assigned, do nothing.
      if (slots(rosterSlot) == Unassigned.ITEM) {
        Output.fullln("not yet assigned")

        // First check the days which this roster slot corresponds to.
        val inventorySlots: List[Int] = slotDays(rosterSlot) match {
          case i: Int => List(i)
          case is: List[Int] => is
        }

        // Next try to find a spot to go here.  We bail out of these
        // nested loops after writing one spot into this slot.
        returning[Unit] {

          // First check whether we can place another instance of what
          // we last placed, and whether these two slots are contiguous.
          if (Math.abs(rosterSlot - lastSlot) == 1)
            then lastSpot match {
              case _: Unassigned => false
              case isSpot: Spot => {
                Output.full(s"  - Trying last used ${isSpot.tag}...")
                if inventorySlots.map(dailyInventories(_).contains(isSpot))
                     .fold(true)(_ && _)
                then {
                  if (nearSameGroup(rosterSlot, isSpot))
                    then {
                      Output.fullln("too close to groupmate")
                    }
                    else {
                      Output.fullln("written")

                      // Remove the spot from consideration from the
                      // days covered here.
                      inventorySlots.map((i) => {
                        dailyInventories(i) = dailyInventories(i) - isSpot
                      })

                      slots(rosterSlot) = lastSpot
                      lastSlot = rosterSlot

                      throwReturn({})
                    }
                } else {
                  Output.fullln("not valid on slot day")
                }
              }
            } else {
              Output.fullln("  - Discontinuous slots; not checking last spot ${lastSpot.tag}")
            }

          // Otherwise search (greedily) for a spot that fits.
          // For every day covered by this roster slot:
          for (candInvSlot <- inventorySlots) {

            // For every spot available for that day:
            for (candSpot <- dailyInventories(candInvSlot)) {
              Output.full(s"  - Trying ${candSpot.tag}...")

              // If that spot is valid on every day covered by
              // this roster slot,
              if (inventorySlots
                    .map((n) => candSpot.validOn(startDate.plusDays(n)))
                    .fold(true)(_ && _))
                then {
                  if (nearSameGroup(rosterSlot, candSpot))
                    then {
                      Output.fullln("too close to groupmate")
                    }
                    else {

                      // Then write the spot into the slot
                      slots(rosterSlot) = candSpot

                      // Remove the spot from consideration from the
                      // days covered here.
                      inventorySlots.map((i) => {
                        dailyInventories(i) = dailyInventories(i) - candSpot
                      })

                      // Note what we have now written.
                      lastSpot = candSpot
                      lastSlot = candInvSlot

                      Output.fullln("written")
                      throwReturn({})
                    }
                } else {
                  Output.fullln("not in slot inventories")
                }
            }
          }
        }
      } else {
        Output.fullln("already filled")
      }
    }
  }
  /** Fill in the context of a weekly series of blockout patterns.
    *  @param bank Available [[Spot]]s.
    *  @param blackoutSpec Bundled control of blackout information.
    */
  def fillByBlackoutTimes(
    bank: SpotBank, mixer: AssortmentSchedule)(
    using blackoutSpec: BlackoutSpec):
      Unit = {
    val spotList =
      mixer.getSortedList(startDate, bank).filter(_ != blackoutSpec.blankSpot)
    var state: BlackoutFillState =
      BlackoutNextSpot(unassignedRanges, spotList, blackoutSpec.readingsPer)
    // println(s"\n\nInitial state: $state")

    while (state.more) {
      state = state.step
    }

    // println(s"\nfillByBlackoutTimes $spotList")
    // fillByBlackoutTimes(
    //  unassignedRanges, spotList, blackoutSpec.readingsPer, blackoutSpec)
  }

  trait BlackoutFillState {
    def more: Boolean
    def step(using blackoutSpec: BlackoutSpec): BlackoutFillState
  }

  case class BlackoutNextSpot(
    ranges: List[(Int, Int)],
    spots: List[Spot],
    readingsToSchedule: Int)
      extends BlackoutFillState {
    override def more: Boolean = ranges.nonEmpty
    override def step(using blackoutSpec: BlackoutSpec): BlackoutFillState = {
      // println(this)
      ranges match {
        case Nil => throw new IllegalStateException("Stepped exhausted state")
        case (firstIdx, lastIdx) :: otherRanges => spots match {
          case Nil => {
            // println("- Transition to defaults")
            BlackoutDefaults0(ranges)
          }
          case firstSpot :: restSpots => {
            // println(s"- Place $firstSpot")
            BlackoutSpotProgress(
              firstIdx, lastIdx, otherRanges, List.empty, firstSpot, restSpots,
              readingsToSchedule)
          }
        }
      }
    }
  }

  case class BlackoutSpotProgress(
    lo: Int, hi: Int,
    otherRanges: List[(Int, Int)],
    skipped: List[(Int, Int)],
    spot: Spot,
    otherSpots: List[Spot],
    readingsToSchedule: Int)
      extends BlackoutFillState {
    override def more: Boolean = true
    override def step(using blackoutSpec: BlackoutSpec): BlackoutFillState = {
      // println(this)
      if (readingsToSchedule <= 0) then {
        // println("- no more readings needed")
        BlackoutNextSpot(
          skipped.reverse ++ ((lo, hi) :: otherRanges), otherSpots,
          blackoutSpec.readingsPer)
      }

      else if (hi < lo) then {
        otherRanges match {
          case Nil => {
            // println("- give up on this spot")
            BlackoutNextSpot(skipped.reverse, otherSpots, readingsToSchedule)
          }
          case (lo0, hi0) :: rs => {
            // println("- next range")
            BlackoutSpotProgress(lo0, hi0, rs,
              skipped, spot, otherSpots, readingsToSchedule)
          }
        }

      } else if (blackoutSpec.spotBlackout(spot, lo)) then {
        // println(s"- blacked out")
        val nextSkipped = skipped match {
          // TODO requeue lo
          case (lo1, hi1) :: otherSkipped if hi1 + 1 == lo =>
            (lo1, lo) :: otherSkipped
          case _ => (lo, lo) :: skipped
        }
        BlackoutSpotProgress(
          1 + lo, hi, otherRanges, nextSkipped, spot,
          otherSpots, readingsToSchedule)

      } else {
        // println(s"- yes")
        set(lo, spot)
        BlackoutSpotProgress(
          1 + lo, hi, otherRanges, skipped, spot,
          otherSpots,
          readingsToSchedule - blackoutSpec.readingsAt(lo))
      }
    }
  }

  case class BlackoutDefaults0(
    ranges: List[(Int, Int)])
      extends BlackoutFillState {
    override def more: Boolean = ranges.nonEmpty
    override def step(using blackoutSpec: BlackoutSpec): BlackoutFillState = {
      // println(this)
      ranges match {
        case Nil => throw new IllegalStateException("Stepped exhausted state")
        case (lo, hi) :: otherRanges => {
          // println(s"- To stepwise for this ranges")
          BlackoutDefaults(lo, hi, otherRanges)
        }
      }
    }
  }

  case class BlackoutDefaults(
    lo: Int, hi: Int,
    otherRanges: List[(Int, Int)]) extends BlackoutFillState {
    override def more: Boolean = (lo <= hi || otherRanges.nonEmpty)
    override def step(using blackoutSpec: BlackoutSpec): BlackoutFillState = {
      // println(this)
      if (lo <= hi) then {
        // println(s"- Add default here")
        set(lo, blackoutSpec.blankSpot)
        BlackoutDefaults(1 + lo, hi, otherRanges)
      } else {
        // println(s"- Check for more ranges for default")
        BlackoutDefaults0(otherRanges)
      }
    }
  }

  def slotOrder: List[Int]

  /** Fill in unassigned slots with [[Spot]]s drawn from the
    * given bank for `date`.
    */
  def fillByAssortment(bank: SpotBank, mixer: AssortmentSchedule): Unit =
    completeWith(mixer.getSortedList(startDate, bank))

  /** Fill in unassigned slots with [[Spot]]s drawn (in order) from the
    * given list.
    */
  def completeWith(spots: List[Spot]): Unit =
    completeWith(unassignedRanges, spots, spots)

  /** Recursive helper method for [[#completeWith]]. */
  private def completeWith(
    ranges: List[(Int, Int)], spots: List[Spot], reset: List[Spot]):
      Unit = {
    // println(s"completeWith($ranges)")
    ranges match {
      case Nil => { }
      case (first, last) :: rest =>
        completeWith(first, last, rest, spots, reset)
    }
  }

  /** Recursive helper method for [[#completeWith]]. */
  private def completeWith(
    first: Int, last: Int, ranges: List[(Int, Int)],
    spots: List[Spot], reset: List[Spot]):
      Unit = {
    // println(s"completeWith($first, $last, $ranges, $spots)")
    spots match {
      case Nil => throw new IllegalArgumentException("Exhausted spots list")
      case spot :: otherSpots => {
        val len = blockPolicy(first, last)
        // println(s"  policy($first, $last) = $len")
        for(i <- 0 until len) {
        // println(s"  ${i + first} <- ${spot.tag}")
          set(i + first, spot)
        }

        val nextSpots = otherSpots match {
          case Nil => reset
          case _ => otherSpots
        }

        if first + len > last
        then completeWith(ranges, nextSpots, reset)
        else completeWith(first + len, last, ranges, nextSpots, reset)
      }
    }
  }
}

trait BlackoutSpec {
  def spotBlackout(spot: Spot, slotIdx: Int): Boolean
  def readingsAt(slotIdx: Int): Int
  def readingsPer: Int
  def blankSpot: Spot
}
