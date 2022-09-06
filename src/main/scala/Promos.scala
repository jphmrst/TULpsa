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
import scala.collection.mutable.HashMap

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

  // TODO --- use the blackout scheduler instead of the assortment
  // scheduler.  Then get rid of any other assortment artifacts.
  override protected
  def complete(builder: PromoRosterBuilder): Unit = {
    given blackoutSpec: BlackoutSpec = builder
    builder.fillByDayMatch(PromoShortTermSpots)
    builder.fillByBlackoutTimes(PromoLongTermSpots, PromoScheduling)
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
) with BlackoutSpec {
  import SlotGlob.*
  val slotOrders: List[List[Int]] = List(
    List(
      0, 4, 8,
      21, 22, 23,
      12, 16, 20,
      24, 25, 26,
      1, 5, 9, 13, 17,
      27, 28, 29,
      2, 6, 10, 14, 18,
      30, 31, 32,
      3, 7, 11, 15, 19,
      33, 34
    ),
    List(
      21, 22, 23,
      12, 16, 20,
      24, 25, 26,
      1, 5, 9, 13, 17,
      27, 28, 29,
      2, 6, 10, 14, 18,
      30, 31, 32,
      3, 7, 11, 15, 19,
      33, 34,
      0, 4, 8
    ),
    List(
      12, 16, 20,
      24, 25, 26,
      1, 5, 9, 13, 17,
      27, 28, 29,
      2, 6, 10, 14, 18,
      30, 31, 32,
      3, 7, 11, 15, 19,
      33, 34,
      0, 4, 8,
      21, 22, 23
    ),
    List(
      24, 25, 26,
      1, 5, 9, 13, 17,
      27, 28, 29,
      2, 6, 10, 14, 18,
      30, 31, 32,
      3, 7, 11, 15, 19,
      33, 34,
      0, 4, 8,
      21, 22, 23,
      12, 16, 20
    ),
    List(
      1, 5, 9, 13, 17,
      27, 28, 29,
      2, 6, 10, 14, 18,
      30, 31, 32,
      3, 7, 11, 15, 19,
      33, 34,
      0, 4, 8,
      21, 22, 23,
      12, 16, 20,
      24, 25, 26
    ),
    List(
      27, 28, 29,
      2, 6, 10, 14, 18,
      30, 31, 32,
      3, 7, 11, 15, 19,
      33, 34,
      0, 4, 8,
      21, 22, 23,
      12, 16, 20,
      24, 25, 26,
      1, 5, 9, 13, 17
    ),
    List(
      2, 6, 10, 14, 18,
      30, 31, 32,
      3, 7, 11, 15, 19,
      33, 34,
      0, 4, 8,
      21, 22, 23,
      12, 16, 20,
      24, 25, 26,
      1, 5, 9, 13, 17,
      27, 28, 29
    ),
    List(
      30, 31, 32,
      3, 7, 11, 15, 19,
      33, 34,
      0, 4, 8,
      21, 22, 23,
      12, 16, 20,
      24, 25, 26,
      1, 5, 9, 13, 17,
      27, 28, 29,
      2, 6, 10, 14, 18
    ),
    List(
      3, 7, 11, 15, 19,
      33, 34,
      0, 4, 8,
      21, 22, 23,
      12, 16, 20,
      24, 25, 26,
      1, 5, 9, 13, 17,
      27, 28, 29,
      2, 6, 10, 14, 18,
      30, 31, 32
    ),
    List(
      33, 34,
      0, 4, 8,
      21, 22, 23,
      12, 16, 20,
      24, 25, 26,
      1, 5, 9, 13, 17,
      27, 28, 29,
      2, 6, 10, 14, 18,
      30, 31, 32,
      3, 7, 11, 15, 19
    )
  )

  val slotGlobs = Array[Set[SlotGlob]](
    Set(MonPredawn, MonBreakfast, MonAfternoon), // 0 A
    Set(MonMorning, MonAfternoon), // 1 B
    Set(MonMorning, MonAfternoon, MonDinner, MonNight), // 2 C
    Set(TuePredawn, TueBreakfast, TueMorning), // 3 D
    Set(TueMorning, TueAfternoon), // 4 E
    Set(TueAfternoon, TueNight), // 5 F
    Set(WedPredawn, WedBreakfast, WedMorning), // 6 G
    Set(WedMorning, WedAfternoon), // 7 H
    Set(WedDinner, WedNight), // 8 I
    Set(ThuPredawn, ThuBreakfast, ThuMorning), // 9 J
    Set(ThuMorning, ThuAfternoon), // 10 K
    Set(ThuAfternoon, ThuDinner, ThuNight), // 11 L
    Set(FriPredawn, FriBreakfast, FriMorning), // 12 M
    Set(FriMorning, FriAfternoon), // 13 N
    Set(FriAfternoon, FriNight), // 14 O
    Set(SatPredawn, SatMorning), // 15 P
    Set(SatBreakfast, SatMorning, SatAfternoon), // 16 Q
    Set(SatAfternoon, SatDinner, SatNight), // 17 R
    Set(SunPredawn, SunBreakfast, SunMorning), // 18 S
    Set(SunMorning, SunAfternoon), // 19 T
    Set(SunAfternoon, SunDinner, SunNight), // 20 U
    Set(MonPredawn, TuePredawn, WedPredawn, ThuPredawn, FriPredawn, SatPredawn, SunPredawn), // 21
    Set(MonPredawn, TuePredawn, WedPredawn, ThuPredawn, FriPredawn, SatPredawn, SunPredawn), // 22
    Set(MonBreakfast, TueDinner, WedBreakfast, ThuDinner, FriBreakfast, SatDinner, SunBreakfast), // 23
    Set(MonDinner, TueBreakfast, WedDinner, ThuBreakfast, FriDinner, SatBreakfast, SunDinner), // 24
    Set(MonMorning, TueMorning, WedMorning, ThuMorning, FriMorning, SatMorning, SunMorning), // 25
    Set(MonMorning, TueMorning, WedMorning, ThuMorning, FriAfternoon, SatMorning, SunMorning), // 26
    Set(MonAfternoon, TueAfternoon, WedAfternoon, ThuAfternoon, FriAfternoon, SatAfternoon, SunAfternoon), // 27
    Set(MonAfternoon, TueAfternoon, WedAfternoon, ThuAfternoon, FriAfternoon, SatAfternoon, SunAfternoon), // 28
    Set(MonNight, TueDinner, WedNight, ThuNight, FriDinner, SatNight, SunNight), // 29
    Set(MonNight, TueNight, WedNight, ThuNight, FriNight, SatNight, SunNight), // 30
    Set(MonPredawn, TuePredawn, WedPredawn, ThuPredawn, FriPredawn, SatPredawn, SunPredawn), // 31
    Set(MonMorning, WedAfternoon, ThuMorning, FriAfternoon), // 32
    Set(MonPredawn, TuePredawn, WedPredawn, ThuPredawn, FriPredawn), // 33
    Set(TueNight, FriNight, SatPredawn, SunPredawn)  // 34
  )

  override def slotOrder: List[Int] =
    import Spot.getWeekOfCentury
    slotOrders(startDate.getWeekOfCentury() % slotOrders.length)

  override def rosterSlotDateTime(roster: Roster, day: Int, idx: Int):
      LocalDateTime =
    LocalDateTime.of(roster.startDate.plusDays(day), LocalTime.of(idx, 30, 0))

  override val rosterDayCount: Int = 7

  override def rosterDaySlotCount(dayIdx: Int): Int = 24

  override def spotBlackout(spot: Spot, slotIdx: Int): Boolean =
    PromoLongTermSpots.overlaps(spot, slotGlobs(slotIdx))

  override def readingsAt(slotIdx: Int): Int = slotIdx match {
    case 0 => 4
    case 2 => 4
    case 3 => 4
    case 4 => 4
    case 6 => 4
    case 8 => 4
    case 9 => 4
    case 11 => 4
    case 12 => 4
    case 15 => 4
    case 16 => 4
    case 17 => 4
    case 18 => 4
    case 19 => 4
    case 20 => 4
    case 21 => 7
    case 22 => 7
    case 23 => 7
    case 24 => 7
    case 25 => 7
    case 26 => 7
    case 27 => 7
    case 28 => 7
    case 29 => 7
    case 30 => 7
    case 31 => 7
    case 32 => 4
    case 33 => 5
    case 34 => 4
    case _ => 3
  }

  override def readingsPer: Int = 13

  override def blankSpot: Spot = PromoLongTermSpots.noPromo
}

/** Bank holding short-term promos. */
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

/** Bank holding long-term promos. */
object PromoLongTermSpots extends SpotBank("promo-long", PromoScheduling) {
  import Group.*
  import SlotGlob.*
  import scala.language.implicitConversions

  given globs: HashMap[Spot, Set[SlotGlob]] = new HashMap[Spot, Set[SlotGlob]]

  def overlaps(spot: Spot, glob: Set[SlotGlob]) =
    !globs.getOrElse(spot, Set.empty).intersect(glob).isEmpty

  Spot(
    "SociaMedia",
    StandardPromo,
    str("If you want to keep in touch with WTUL's events, playlists, and other updates, check out our social media pages! Our handle on Twitter and Instagram is")
      + online("@WTUL") > comma
      + str("or search for our Facebook page under")
      + online("WTUL New Orleans") > period
      + str("And you can find our website at")
      + online("W T U L dot F M")
      > period,
    start = "2022-01-01",
    alert = "2022-12-05"
  )

  Spot(
    "PSAcall",
    StandardPromo,
    str("Does your group work for the good of New Orleans?  WTUL's public service announcements prioritize charitable activities, human services, and volunteer opportunities.  We also announce public meetings, grassroots organizations, local issue-oriented community activities, and free public lectures and exhibits.")
      + moreWebEmail("W T U L new orleans dot com slash about slash P S A S", "W T U L hyphen P S A at gmail dot com"),
    start = "2022-09-12",
    alert = "2022-12-05"
  )

  Show(
    "DemocracyNow",
    StandardPromo,
    it("Democracy Now")
      + str("airs Mondays, Wednesdays, and Fridays at 9") > am
      + str("right here on WTUL New Orleans.")
      + it("Democracy Now") > comma
      + str("with host Amy Goodman, offers award-winning journalism and fresh and progressive perspectives on national and international issues."),
    start = "2022-01-01",
    alert = "2022-12-05",
    occludes = Set(MonMorning, WedMorning, FriMorning)
  )

  Show(
    "Jazz",
    StandardPromo,
    str("Every weekday evening, tune your radios to 91.5FM for sweet jazz music from artists new and old, local and global.  You don't want to miss it!  Listen to the World of Jazz every weeknight, Monday through Friday, from 6 to 8")
      > pm
      + str("on WTUL New Orleans."),
    start = "2022-01-01",
    alert = "2022-12-05",
    occludes = Set(MonDinner, TueDinner, WedDinner, ThuDinner, FriDinner)
  )

  Show(
    "Surf",
    StandardPromo,
    str("Storm Surge of Reverb serves up a jumbo portion of high-energy surf and instrumental rock-and-roll music every Monday from 4 to 6")
      > pm > period
      + str("It's not just Wipe Out on repeat, it's vintage and new bands from around the world, playing wild, strange and wonderful rock-and-roll instrumentals. That's Storm Surge of Reverb, every Monday from 4 to 6")
      > pm > period,
    start = "2022-01-01",
    alert = "2022-12-05",
    occludes = Set(MonAfternoon)
  )

  Show(
    "Country",
    StandardPromo,
    str("You can catch WTUL's Country Show every Sunday from 2 to 4")
      > pm > period
      + str("Tune in for two hours of folk, blues, cowpunk, western swing, that rockabilly thing, outlaw and everything in between, right here on WTUL."),
    start = "2022-01-01",
    alert = "2022-12-05",
    occludes = Set(SunAfternoon)
  )

  Show(
    "TwentyTwentyOne",
    StandardPromo,
    str("Sunday Nights at 8")
      > pm > comma
      + str("WTUL's classical and experimental program ")
      + phonetic(str("20/21"), "Twenty Twenty-One")
      + str("rounds out the weekend with four hours of music from the birth of modernism to today. Each week, DJ Pennebaker explores a range of styles and sounds --- from contemporary compositions to 20th century classics --- with excursions into electronic music and the avant-garde.  A Sunday night feature for nearly forty years, 20/21 airs Sunday from 8") > pm + str("to midnight here on WTUL."),
    start = "2022-01-01",
    alert = "2022-12-05",
    occludes = Set(SunNight)
  )

  Show(
    "Cheez",
    StandardPromo,
    str("Tune in to WTUL every Sunday from 6 to 10") > am
      + str("for the Cheez Muzik Show.  What is Cheez, you may ask? Space Music, Ambient, Berlin School, Downtempo, New Age. This music is cheesy! Tune into the Cheez Muzik Show every Sunday morning from 6 to 10")
      > am > period,
    start = "2022-01-01",
    alert = "2022-12-05",
    occludes = Set()
  )

  Show(
    "FolkAmericana",
    StandardPromo,
    str("Every Saturday afternoon, WTUL brings you four hours of Americana music. At noon, the folk show with host Mark T brings you singer-songwriters, fiddlers, bluegrass, and more. The folk show is followed by the Americana show at 2")
      > pm > comma
      + str("dusting off the old and bringing you new Americana, rockabilly, and alt-country music. Tune in every Saturday from noon to four for your weekly dose of down-home roots music right here on WTUL New Orleans."),
    start = "2022-01-01",
    alert = "2022-12-05",
    occludes = Set(SatAfternoon)
  )

  Show(
    "Kids",
    StandardPromo,
    str("Every Saturday morning, the Kid's Show with DJ Liz E brings you family-friendly programming to start your day off right. Join us for music and stories for kids of all ages. It's a pajama dance party right in your living room, and it's only here on WTUL, Saturdays from 8 to 10") > am > period,
    start = "2000-01-01",
    alert = "2999-12-31",
    occludes = Set(SatMorning)
  )

  val noPromo = Spot(
    "NoPromo",
    NoPromo,
    str("(No promo in this slot this week)"),
    start = "2000-01-01",
    alert = "2999-12-31"
  )
}

object Show {
  def apply(
    tag: String,
    group: Group,
    text: StructText,
    copresent: Option[String] = None,
    start: LocalDate,
    alert: LocalDate,
    end: Option[LocalDate] = None,
    occludes: Set[SlotGlob] = Set.empty,
    sourceContacts: Seq[String] = Seq(),
    sourceURL: Seq[String] = Seq(),
    sourceNote: Option[String] = None,
    orgName: Option[String] = None,
    groupGainMultiplier: Double = 1.0,
    variantGroup: Int = Spot.nextVariantGroup,
    boost: Double = 0.0,
    previousAlerts: Seq[LocalDate] = Seq()
  )(
    using addSpot: (Spot) => Unit,
    globs: HashMap[Spot, Set[SlotGlob]]
  ): Spot = {
    val result = Spot(
      tag, group, text, copresent, start, alert, end,
      sourceContacts, sourceURL, sourceNote, orgName, groupGainMultiplier,
      variantGroup, boost, previousAlerts
    )
    globs(result) = occludes
    result
  }
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

/** General times of day in the week where a slot might be.  Used to
  * bar individual [[Spot]]s from certain times of the week.
  */
enum SlotGlob {
  case MonPredawn, MonBreakfast, MonMorning, MonAfternoon,
    MonDinner, MonNight
  case TuePredawn, TueBreakfast, TueMorning, TueAfternoon,
    TueDinner, TueNight
  case WedPredawn, WedBreakfast, WedMorning, WedAfternoon,
    WedDinner, WedNight
  case ThuPredawn, ThuBreakfast, ThuMorning, ThuAfternoon,
    ThuDinner, ThuNight
  case FriPredawn, FriBreakfast, FriMorning, FriAfternoon,
    FriDinner, FriNight
  case SatPredawn, SatBreakfast, SatMorning, SatAfternoon,
    SatDinner, SatNight
  case SunPredawn, SunBreakfast, SunMorning, SunAfternoon,
    SunDinner, SunNight
}
