// Event.scala --- (c) 2021 John Maraist
// Part of the WTUL Roster Generator
//
// This file is made available under the GNU GPL version 3; see the
// LICENSE file in this distribution or https://fsf.org/ for more
// details.

package org.maraist.wtulrosters
import java.time.LocalDate
import org.maraist.wtulrosters.Group.Events

object Event {
  def apply(
    baseTag: String,
    template: String,
    eventDate: LocalDate,
    givenStart: Option[LocalDate] = None,
    spotsCopresent: Option[String] = None,
    spotsSourceContacts: Seq[String] = Seq(),
    spotsSourceURL: Seq[String] = Seq(),
    spotsSourceNote: Option[String] = None,
    spotsGroupGainMultiplier: Double = 1.0,
    spotsVariantGroup: Int = Spot.nextVariantGroup,
    spotsBoost: Double = 0.0
  )(using addSpot: (Spot) => Unit) = {
    val startDate: LocalDate =
      givenStart.getOrElse(eventDate.minusDays(30))

    val tomorrow = eventDate.minusDays(1)
    val thisWeek = eventDate.minusDays(7)
    val nextWeek = eventDate.minusDays(13)
    val thisMonth =
      LocalDate.of(startDate.getYear(), startDate.getMonth(), 1)

    if tomorrow.isBefore(startDate)
    then throw new IllegalArgumentException(
      "Start date for " + baseTag + " is not before event date.")

    Spot(baseTag + "Tmrw", Events,
      placeTemplate(template,
        "tomorrow",
        "Tomorrow",
        "tomorrow,",
        "tomorrow."),
      spotsCopresent,
      tomorrow,
      LocalDate.MAX,
      Some(tomorrow),
      spotsSourceContacts,
      spotsSourceURL,
      spotsSourceNote,
      spotsGroupGainMultiplier,
      spotsVariantGroup,
      spotsBoost
    )
  }

  def placeTemplate(
    template: String,
    whenLC: String,
    whenCap: String,
    whenComma: String,
    whenPeriod: String
  ): String = ???
}
