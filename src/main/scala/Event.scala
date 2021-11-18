// Event.scala --- (c) 2021 John Maraist
// Part of the WTUL Roster Generator
//
// This file is made available under the GNU GPL version 3; see the
// LICENSE file in this distribution or https://fsf.org/ for more
// details.

package org.maraist.wtulrosters
import java.util.Locale.US
import java.time.LocalDate
import java.time.format.TextStyle.FULL
import org.maraist.structext.fromString
import org.maraist.wtulrosters.Group.Events

/** Creating [[Spot]]s for announcing an event in the time beforehand.
  */
object Event {

  /** Creates one or more [[Spot]]s for announcing an event in the time
    * beforehand, using correct relative time references in the
    * different spots.
    * @param baseTag A short [[String]] for naming this announcement.
    * Each spot will mutate this string in a different way.
    * @param template Template text for each [[Spot]]'s announcement.
    * @param eventDate The date of the event.
    * @param givenStart Start date of this announcement.
    * @param spotsCopresent If present, a string representing the
    * group or person which should be credited as a co-presenter of
    * the announcement.
    * @param spotsSourceContacts A list of names or email addresses of
    * the contact people at the organization associated with this
    * event.
    * @param spotsSourceURL A list of URLs associated with the
    * sourcing of this announcement.
    * @param spotsSourceNote If present, a note about the sourcing of
    * this spot.
    * @param spotsGroupGainMultiplier A factor to be applied to the
    * gain taken from the group when applied to this spot.
    * @param spotsVariantGroup An identification number to be shared
    * among announcements of the same source.
    * @param spotsBoost An upwards compression factor to be applied to
    * this spot in any [[Assortment]].
    */
  def apply(
    baseTag: String,
    template: String,
    eventDate: LocalDate,
    givenStart: Option[LocalDate] = None,
    spotsCopresent: Option[String] = None,
    spotsSourceContacts: Seq[String] = Seq(),
    spotsSourceURL: Seq[String] = Seq(),
    spotsSourceNote: Option[String] = None,
    spotsSourceName: Option[String] = None,
    spotsGroupGainMultiplier: Double = 1.0,
    spotsVariantGroup: Int = Spot.nextVariantGroup,
    spotsBoost: Double = 0.8
  )(using addSpot: (Spot) => Unit): Unit = {
    Output.fullln(s"Event group $baseTag")

    val startDate: LocalDate =
      givenStart.getOrElse(eventDate.minusDays(30))

    val tomorrow = eventDate.minusDays(1)
    Output.fullln(s"- tomorrow $tomorrow")
    val thisWeek = eventDate.minusDays(6)
    Output.fullln(s"- thisWeek $thisWeek")
    val nextWeek = eventDate.minusDays(9)
    Output.fullln(s"- nextWeek $nextWeek")
    val thisMonth = LocalDate.of(eventDate.getYear(), eventDate.getMonth(), 1)
    Output.fullln(s"- thisMonth $thisMonth")

    if tomorrow.isBefore(startDate)
    then throw new IllegalArgumentException(
      "Start date for " + baseTag + " is not before event date.")

    val eventCardinalDate = "the " + cardinal(eventDate.getDayOfMonth())
    val eventWeekDay = eventDate.getDayOfWeek().getDisplayName(FULL, US)
    val tomorrowCore = s"omorrow, $eventWeekDay $eventCardinalDate"
    Spot(baseTag + "Tmrw", Events,
      placeTemplate(template,
        "T" + tomorrowCore + ",",
        "t" + tomorrowCore + ",",
        "t" + tomorrowCore + ",",
        "t" + tomorrowCore + "."),
      spotsCopresent,
      tomorrow,
      LocalDate.MAX,
      Some(tomorrow),
      spotsSourceContacts,
      spotsSourceURL,
      spotsSourceNote,
      spotsSourceName,
      spotsGroupGainMultiplier,
      spotsVariantGroup,
      boost = spotsBoost
    )

    // Detect when the announcement is for one day only, and return.
    if tomorrow.isEqual(startDate) then return

    // Spot for "This XXX, the Nth"
    val startsThisWeek: Boolean = thisWeek.isBefore(startDate)
    val thisWeekStartDate = startsThisWeek match {
      case true => startDate
      case false => thisWeek
    }
    val thisWeekCore = "his " + eventWeekDay + ", " + eventCardinalDate
    Spot(baseTag + "This", Events,
      placeTemplate(template,
        "T" + thisWeekCore + ",",
        "t" + thisWeekCore + ",",
        "t" + thisWeekCore + ",",
        "t" + thisWeekCore + "."),
      spotsCopresent,
      thisWeekStartDate,
      LocalDate.MAX,
      Some(tomorrow.minusDays(1)),
      spotsSourceContacts,
      spotsSourceURL,
      spotsSourceNote,
      spotsSourceName,
      spotsGroupGainMultiplier,
      spotsVariantGroup,
      boost = spotsBoost
    )
    if startsThisWeek then return

    // Spot for "Next XXX, the Nth"
    val startsNextWeek: Boolean = nextWeek.isBefore(startDate)
    val nextWeekStartDate = startsNextWeek match {
      case true => startDate
      case false => nextWeek
    }
    val nextWeekCore = "ext " + eventWeekDay + ", " + eventCardinalDate
    Spot(baseTag + "Next", Events,
      placeTemplate(template,
        "N" + nextWeekCore + ",",
        "n" + nextWeekCore + ",",
        "n" + nextWeekCore + ",",
        "n" + nextWeekCore + "."),
      spotsCopresent,
      nextWeekStartDate,
      LocalDate.MAX,
      Some(thisWeek.minusDays(1)),
      spotsSourceContacts,
      spotsSourceURL,
      spotsSourceNote,
      spotsSourceName,
      spotsGroupGainMultiplier,
      spotsVariantGroup,
      boost = spotsBoost
    )
    if startsNextWeek then return

    // Spot for "On XXX the Nth" --- more than two weeks before the date
    // but this month
    val earlierEnds = if (thisMonth.isBefore(nextWeek)) {
      val startsThisMonth: Boolean = thisMonth.isBefore(startDate)
      val thisMonthStartDate = startsThisMonth match {
        case true => startDate
        case false => thisMonth
      }
      val thisMonthCore = "n " + eventWeekDay + " " + eventCardinalDate
      Spot(baseTag + "Month", Events,
        placeTemplate(template,
          "O" + thisMonthCore + ",",
          "o" + thisMonthCore + ",",
          "o" + thisMonthCore + ",",
          "o" + thisMonthCore + "."),
        spotsCopresent,
        thisMonthStartDate,
        LocalDate.MAX,
        Some(nextWeek.minusDays(1)),
        spotsSourceContacts,
        spotsSourceURL,
        spotsSourceNote,
        spotsSourceName,
        spotsGroupGainMultiplier,
        spotsVariantGroup,
        boost = spotsBoost
      )
      if startsThisMonth then return
      thisMonth.minusDays(1)
    } else {
      nextWeek.minusDays(1)
    }

    // Spot for "On DDD, MMM Nth"
    val nextMonthCore = (
      "n " + eventWeekDay + ", " +
        eventDate.getMonth().getDisplayName(FULL, US) +
        " " + eventCardinalDate)
    Spot(baseTag + "Far", Events,
      placeTemplate(template,
        "O" + nextMonthCore + ",",
        "o" + nextMonthCore + ",",
        "o" + nextMonthCore + ",",
        "o" + nextMonthCore + "."),
      spotsCopresent,
      startDate,
      LocalDate.MAX,
      Some(earlierEnds),
      spotsSourceContacts,
      spotsSourceURL,
      spotsSourceNote,
      spotsSourceName,
      spotsGroupGainMultiplier,
      spotsVariantGroup,
      boost = spotsBoost
    )
  }

  /** Substitute the appropriate relative time references in the given
    * text.
    */
  def placeTemplate(
    template: String,
    whenCap: String,
    whenLC: String,
    whenComma: String,
    whenPeriod: String
  ): String = template
    .replace("%%When%%", whenCap)
    .replace("%%when%%", whenLC)
    .replace("%%when,%%", whenComma)
    .replace("%%when.%%", whenPeriod)

  /** Format an integer as a cardinal reference.  We only need to go up
    * to the highest date number for any month.
    */
  private def cardinal(n: Int): String = n match {
    case 1 => "first"
    case 2 => "second"
    case 3 => "third"
    case 4 => "fourth"
    case 5 => "fifth"
    case 6 => "sixth"
    case 7 => "seventh"
    case 8 => "eighth"
    case 9 => "ninth"
    case 10 => "tenth"
    case 11 => "eleventh"
    case 12 => "twelvth"
    case 13 => "thirteenth"
    case 14 => "fourteenth"
    case 15 => "fifteenth"
    case 16 => "sixteenth"
    case 17 => "seventeenth"
    case 18 => "eighteenth"
    case 19 => "ninteenth"
    case 20 => "twentieth"
    case 21 => "twenty-first"
    case 22 => "twenty-second"
    case 23 => "twenty-third"
    case 24 => "twenty-fourth"
    case 25 => "twenty-fifth"
    case 26 => "twenty-sixth"
    case 27 => "twenty-seventh"
    case 28 => "twenty-eighth"
    case 29 => "twenty-ninth"
    case 30 => "thirtieth"
    case 31 => "thirty-first"
    case n => throw new IllegalArgumentException("No date cardinal " + n)
  }
}
