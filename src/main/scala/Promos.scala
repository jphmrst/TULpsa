// Promos.scala --- (c) 2021 John Maraist
// Part of the WTUL Roster Generator
//
// This file is made available under the GNU GPL version 3; see the
// LICENSE file in this distribution or https://fsf.org/ for more
// details.

// To skip to the actual Promo spots: search for "short-term" or
// "long-term".

package org.maraist.wtulrosters
import java.time.{LocalDate, LocalDateTime, LocalTime}
import java.time.format.DateTimeFormatter
import org.maraist.structext.{SpeakAs, StructText, fromString}
import org.maraist.structext.StructText.*

object PromoUtils {
  def basePolicy(slot: Int): Int =
    if slot < 14 then 3 else
      if slot < 29 then 5 else
        if slot < 46 then 3
          else 5
}

/** How we generate Promo rosters. */
object PromoRosters extends RosterType("promos-") {

  override def init(): Unit = {
    PromoShortTermSpots.init()
    PromoLongTermSpots.init()
    PromoScheduling.init()
  }

  override protected type RBuilder = PromoRosterBuilder

  override protected
  def newBuilder(date: LocalDate): PromoRosterBuilder =
    new PromoRosterBuilder(date)

  override protected
  def complete(builder: PromoRosterBuilder): Unit = {
    builder.fillByDayMatch(PromoShortTermSpots)
    builder.fillByAssortment(PromoLongTermSpots, PromoScheduling)
  }
}

val promoLabels: Array[String] = Array(
  "A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M",
  "N", "O", "P", "Q", "R", "S", "T", "U", "AA", "BB", "CC", "DD",
  "EE", "FF", "GG", "HH", "II", "JJ", "KK", "LL", "MM", "NN")

/** Specialization of [[RosterBuilder]] with the specifics for Promos.
  * @param startDate The first date of the week to be covered by this
  * roster.
  */
class PromoRosterBuilder(startDate: LocalDate)
    extends RosterBuilder("Promo", startDate, promoLabels.size,
      PromoScheduling,
      "WTUL 91.5\\textsc{fm} --- Promo roster",
      "Promo \\#",
      "Please report typos, expired spots, or other problems with Promos to \\textsl{wtul-psa@gmail.com}\\,.",
      (x: Int) => promoLabels(x-1),
      commonPreamble("promos"),
      DateTimeFormatter.ofPattern("MMMM d, yyyy, h:mm'{\\small 'a'}'"),
      "Promo-",
      (first, last) => {
        (last - first) match {
          case 0 => 1
          case _ => 2
        }
      },
      Array[Int | List[Int]](
        0, 0, 0,  // A B C
        1, 1, 1,  // D E F
        2, 2, 2,  // G H I
        3, 3, 3,  // J K L
        4, 4, 4,  // M N O
        5, 5, 5,  // P Q R
        6, 6, 6,  // S T U
        List(0, 1, 2, 3, 4, 5, 6),  // AA
        List(0, 1, 2, 3, 4, 5, 6),  // BB
        List(0, 1, 2, 3, 4, 5, 6),  // CC
        List(0, 1, 2, 3, 4, 5, 6),  // DD
        List(0, 1, 2, 3, 4, 5, 6),  // EE
        List(0, 1, 2, 3, 4, 5, 6),  // FF
        List(0, 1, 2, 3, 4, 5, 6),  // GG
        List(0, 1, 2, 3, 4, 5, 6),  // HH
        List(0, 1, 2, 3, 4, 5, 6),  // II
        List(0, 1, 2, 3, 4, 5, 6),  // JJ
        List(0, 1, 2, 3, 4, 5, 6),  // KK
        List(0, 2, 3, 4),           // LL
        List(0, 1, 2, 3, 4),        // MM
        List(1, 4, 5, 6)            // NN
      ),
      Array[Array[Int]](
        Array[Int](21, 31, 0, 33, 22, 0, 23, 0, 32, 1, 25, 2, 26, 1, 27, 0, 28, 1, 24, 2, 2, 29, 2, 30),
        Array[Int](3, 21, 31, 22, 3, 33, 3, 24, 3, 25, 4, 4, 5, 28, 26, 4, 27, 4, 23, 29, 5, 30, 5, 34),
        Array[Int](21, 31, 6, 22, 6, 33, 6, 23, 7, 25, 6, 8, 26, 7, 27, 7, 28, 32, 24, 8, 29, 8, 30, 8),
        Array[Int](31, 9, 21, 9, 33, 22, 24, 9, 32, 10, 25, 9, 26, 10, 28, 11, 27, 10, 23, 11, 11, 29, 11, 30),
        Array[Int](12, 31, 21, 12, 22, 33, 23, 12, 25, 13, 12, 26, 13, 32, 13, 27, 28, 14, 29, 24, 14, 30, 34, 14),
        Array[Int](34, 21, 15, 31, 22, 15, 16, 24, 15, 16, 25, 15, 16, 26, 28, 17, 27, 16, 23, 17, 29, 17, 17, 30),
        Array[Int](21, 18, 31, 18, 22, 34, 18, 23, 19, 25, 18, 19, 26, 20, 19, 28, 19, 27, 24, 20, 29, 20, 30, 20)),
      false,
      "green!30"
) {
  val slotOrders: List[List[Int]] = List(
    List(
      0, 1, 2, 3, 4,
      5, 6, 7, 8, 9,
      10, 11, 12, 13, 14,
      15, 16, 17, 18, 19,
      20, 21, 22, 23, 24,
      25, 26, 27, 28, 29,
      30, 31, 32, 33, 34),
    List(
      30, 31, 32, 33, 34,
      0, 1, 2, 3, 4,
      5, 6, 7, 8, 9,
      10, 11, 12, 13, 14,
      15, 16, 17, 18, 19,
      20, 21, 22, 23, 24,
      25, 26, 27, 28, 29),
    List(
      25, 26, 27, 28, 29,
      30, 31, 32, 33, 34,
      0, 1, 2, 3, 4,
      5, 6, 7, 8, 9,
      10, 11, 12, 13, 14,
      15, 16, 17, 18, 19,
      20, 21, 22, 23, 24),
    List(
      20, 21, 22, 23, 24,
      25, 26, 27, 28, 29,
      30, 31, 32, 33, 34,
      0, 1, 2, 3, 4,
      5, 6, 7, 8, 9,
      10, 11, 12, 13, 14,
      15, 16, 17, 18, 19),
    List(
      15, 16, 17, 18, 19,
      20, 21, 22, 23, 24,
      25, 26, 27, 28, 29,
      30, 31, 32, 33, 34,
      0, 1, 2, 3, 4,
      5, 6, 7, 8, 9,
      10, 11, 12, 13, 14),
    List(
      10, 11, 12, 13, 14,
      15, 16, 17, 18, 19,
      20, 21, 22, 23, 24,
      25, 26, 27, 28, 29,
      30, 31, 32, 33, 34,
      0, 1, 2, 3, 4,
      5, 6, 7, 8, 9),
    List(
      5, 6, 7, 8, 9,
      10, 11, 12, 13, 14,
      15, 16, 17, 18, 19,
      20, 21, 22, 23, 24,
      25, 26, 27, 28, 29,
      30, 31, 32, 33, 34,
      0, 1, 2, 3, 4)
  )

  override def slotOrder: List[Int] =
    import Spot.getWeekOfCentury
    slotOrders(startDate.getWeekOfCentury() % slotOrders.length)

  override def rosterSlotDateTime(roster: Roster, day: Int, idx: Int):
      LocalDateTime =
    LocalDateTime.of(roster.startDate.plusDays(day), LocalTime.of(idx, 30, 0))

  override val rosterDayCount: Int = 7

  override def rosterDaySlotCount(dayIdx: Int): Int = 24
}

/** Bank holding short-term Promos. */
object PromoShortTermSpots extends SpotBank("promo-short", PromoScheduling) {
  import Group.*
  import scala.language.implicitConversions

//  // Dummy spot
//  Event("sierraMtgJune22",
//    str("The next meeting of the Orleans Sierra Club will be") +
//      blank("when") +
//      str("at 6:30") + pm > period +
//      str("Zach Kopkin, the Sierra Club's regional organizer, will discuss the Sierra Club’s ``Beyond Dirty Fuels'' Campaign.  The campaignaims to stop the expansion of dirty fossil fuels infrastructure projects, bring about a safer climate future, and help open a path for clean, renewable energy.  "
//        + "  The meeting will be at First Unitarian Universalist, 5212 South Claiborne.") +
//      moreWebEmail("sierra club dot O R G slash delta", "doc tim sierra at gmail dot com"),
//    "2000-06-12",
//    spotsSourceURL = Seq("https://www.sierraclub.org/delta/new-orleans-group")
//  )

}

/** Bank holding long-term Promos. */
object PromoLongTermSpots extends SpotBank("promo-long", PromoScheduling) {
  import Group.*
  import scala.language.implicitConversions

  Spot(
    "SociaMedia",
    StandardPromo,
    str("If you want to keep in touch with WTUL's events, playlists, and other updates, check out our social media pages! Join us @WTUL on twitter and instagram, and at WTUL New Orleans on Facebook. You can also check out our website at wtul.fm!"),
    start = "2022-01-01",
    alert = "2022-12-05"
  )

  Spot(
    "DemocracyNow",
    StandardPromo,
    str("Democracy Now airs Mondays, Wednesdays, and Fridays at 9AM right here on WTUL New Orleans. Democracy Now, with host Amy Goodman, offers award-winning journalism and fresh and progressive perspectives on national and international issues."),
    start = "2022-01-01",
    alert = "2022-12-05"
  )

  Spot(
    "Jazz",
    StandardPromo,
    str("Every weekday evening tune your radios to WTUL New Orleans 91.5 to hear some sweet Jazz music from  artists new and old, local and global; you don't want to miss it! Mondays, Wednesdays and Thursdays 6-8pm and Tuesdays and Fridays 4-6pm."),
    start = "2022-01-01",
    alert = "2022-12-05"
  )

  Spot(
    "Surf",
    StandardPromo,
    str("Storm Surge of Reverb serves up a jumbo portion of high-energy surf and instrumental rock-and-roll music every Monday from 4 to 6pm. It's not just Wipe Out on repeat, it's vintage and new bands from around the world, playing wild, strange and wonderful rock-and-roll instrumentals. That's Storm Surge of Reverb, every Monday from 4 to 6pm."),
    start = "2022-01-01",
    alert = "2022-12-05"
  )

  Spot(
    "Country",
    StandardPromo,
    str("You can catch WTUL's country show every Sunday from 2-4pm. Tune in for two hours of folk, blues, cowpunk, western swing, that rockabilly thing, outlaw and everything in between, right here on WTUL."),
    start = "2022-01-01",
    alert = "2022-12-05"
  )

  Spot(
    "TwentyTwentyOne",
    StandardPromo,
    str("Sunday Nights at 8PM, WTUL's classical and experimental program 20/21 (``Twenty Twenty-One''), rounds out the weekend with four hours of music from the birth of modernism to today. Each week, DJ Pennebaker explores a range of styles and sounds --- from contemporary compositions to 20th century classics --- with excursions into electronic music and the avantgarde. A Sunday night feature for nearly 40 years. 20/21 airs Sunday from 8pm to Midnight here on WTUL."),
    start = "2022-01-01",
    alert = "2022-12-05"
  )

  Spot(
    "Cheez",
    StandardPromo,
    str("Tune in to WTUL every Sunday from 6-10 am for the Cheez Muzik Show! What is Cheez, you may ask? Space Music, Ambient, Berlin School, Downtempo, New Age. This music is cheesy! Tune into the Cheez Muzik Show every Sunday morning from 6-10am."),
    start = "2022-01-01",
    alert = "2022-12-05"
  )

  Spot(
    "FolkAmericana",
    StandardPromo,
    str("Every Saturday afternoon, WTUL brings you four hours of Americana music. At Noon, the folk show with host Mark T brings you singer-songwriters, fiddlers, bluegrass, and more. The folk show is followed by the Americana show at 2pm, dusting off the old and bringing you new Americana, rockabilly, and alt-country music. Tune in every Saturday from noon-4 for your weekly dose of down-home roots music right here on WTUL New Orleans."),
    start = "2022-01-01",
    alert = "2022-12-05"
  )

  Spot(
    "Kids",
    StandardPromo,
    str("Every Saturday morning, the Kid's Show with DJ Liz E brings you family-friendly programming to start your day off right. Join us for music and stories for kids of all ages. It's a pajama dance party right in your living room, and it's only here on WTUL, Saturdays from 8-10am."),
    start = "2000-01-01",
    alert = "2999-12-31"
  )

  Spot(
    "NoPromo",
    NoPromo,
    str("(No promo in this slot this week)"),
    start = "2000-01-01",
    alert = "2999-12-31"
  )
}

/** Seasonal scheduling rules for long-term Promos. */
object PromoScheduling extends AssortmentSchedule, Utils.Converters {
  import scala.language.implicitConversions
  import Group.*
  import Assortment.*

  /** Default assortment when nothing else applies */
  private val baseWeights: Map[Group, Double] = Map(
    StandardPromo -> NO_GAIN,
    NoPromo -> NEG_GAIN
  )

  // The default
  Assortment(LocalDate.MIN, always, baseWeights, LocalDate.MAX)
  // See other assortments in PSAs.scala
}
