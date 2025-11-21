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

  // Event(tag, structText, endDate,
  //       givenStart=Some(startDate),
  //       spotsBoost=(0 < x < 1)
  // )

  Event("PublicDomainRemixTwentySix",
    str("WTUL and Tulane Libraries want you to reimagine and remix public domain music. The Public Domain Music Remix contest is a chance for Tulane students to create fresh new works from music that has recently entered or will soon enter the public domain. All submissions are due no later than 11:59") > pm
      + blank("when.")
      + str("Contest details, including how to submit music, are available at")
      + online("library dot tulane dot E D U slash news") > comma
      + str("under") + emph("Public Domain Music Remix Contest") > period
      + str("The contest is only open to Tulane students, but all WTUL listeners will get to enjoy the results!"),
    "2026-01-14",
    givenStart = Some("2025-11-24")
  )

  Event("MarathonMerchNovTwentyFive",
    str("It’s time for our WTUL Marathon Merch Competition! Do you want your design to be a part of WTUL history, and worn by the coolest radio DJs and music nerds in town? Enter our Marathon merch design competition for a chance to win free entry into all of our Marathon events and a full suite of merch with your design on it! Visit")
      + online("W T U L dot F M")
      + str("for more information about design criteria, competition information, and submission guidelines! Deadline to submit is")
      + blank("when."),
    "2025-12-12",
    spotsBoost = 0.96
  )

  Event("TunnelvisionKatrina",
    str("Listen to a special edition of Tunnelvisions at 2")
      > pm
      + blank("when")
      + str("the 20th anniversary of Hurricane Katrina. DJ Dominomnom, a Katrina Kid herself, has interviewed twenty Katrina Kids about the impact of the storm on their lives and what they're looking for in our future as New Orleanians. Tune in to Tunnelvisions") +
      blank("when") + "at 2" > pm
      + str("to hear more."),
    "2025-08-29"
  )

  Event("BackToSchool2024",
    str("WTUL is hosting a Back 2 School Bash") +
      blank("when") +
      str("at The Broadside! Doors at 5, show at 6. This funky lineup is loaded with tunes that will make you feel too cool for school, from Lady Li, Doctors, The Dewdrops, Raph and the Rotation and Planet of the Little Green Men! For more information, you can check our website and social media pages!"),
    "2024-08-22"
  )

  Event("BackToSchool2024b",
    str("WTUL is hosting a Back 2 School Bash") +
      blank("when") +
      str("at The Broadside! Doors at 5, show at 6. This funky lineup is loaded with tunes that will make you feel too cool for school, from Lady Li, Doctors, The Dewdrops, Raph and the Rotation and Planet of the Little Green Men! For more information, you can check our website and social media pages!"),
    "2024-08-22"
  )

  Event("BackToSchool2024c",
    str("WTUL is hosting a Back 2 School Bash") +
      blank("when") +
      str("at The Broadside! Doors at 5, show at 6. This funky lineup is loaded with tunes that will make you feel too cool for school, from Lady Li, Doctors, The Dewdrops, Raph and the Rotation and Planet of the Little Green Men! For more information, you can check our website and social media pages!"),
    "2024-08-22"
  )

  Event("BackToSchool2024d",
    str("WTUL is hosting a Back 2 School Bash") +
      blank("when") +
      str("at The Broadside! Doors at 5, show at 6. This funky lineup is loaded with tunes that will make you feel too cool for school, from Lady Li, Doctors, The Dewdrops, Raph and the Rotation and Planet of the Little Green Men! For more information, you can check our website and social media pages!"),
    "2024-08-22"
  )

  Event("BackToSchool2024e",
    str("WTUL is hosting a Back 2 School Bash") +
      blank("when") +
      str("at The Broadside! Doors at 5, show at 6. This funky lineup is loaded with tunes that will make you feel too cool for school, from Lady Li, Doctors, The Dewdrops, Raph and the Rotation and Planet of the Little Green Men! For more information, you can check our website and social media pages!"),
    "2024-08-22"
  )

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

  Show(
    "BackToSchool2024f",
    StandardPromo,
    str("WTUL is hosting a Back 2 School Bash on Thursday the 22nd at The Broadside! Doors at 5, show at 6. This funky lineup is loaded with tunes that will make you feel too cool for school, from Lady Li, Doctors, The Dewdrops, Raph and the Rotation and Planet of the Little Green Men! There's more information on our website and social media pages!"),
    start = "2024-08-18",
    end = Some("2024-08-22"),
    alert = "2023-08-31",
    occludes = Set(SatDinner)
  )

  Show(
    "BackToSchool2024g",
    StandardPromo,
    str("WTUL is hosting a Back 2 School Bash on Thursday the 22nd at The Broadside! Doors at 5, show at 6. This funky lineup is loaded with tunes that will make you feel too cool for school, from Lady Li, Doctors, The Dewdrops, Raph and the Rotation and Planet of the Little Green Men! There's more information on our website and social media pages!"),
    start = "2024-08-18",
    end = Some("2024-08-22"),
    alert = "2023-08-31",
    occludes = Set(SatDinner)
  )

  Show(
    "BackToSchool2024h",
    StandardPromo,
    str("WTUL is hosting a Back 2 School Bash on Thursday the 22nd at The Broadside! Doors at 5, show at 6. This funky lineup is loaded with tunes that will make you feel too cool for school, from Lady Li, Doctors, The Dewdrops, Raph and the Rotation and Planet of the Little Green Men! There's more information on our website and social media pages!"),
    start = "2024-08-18",
    end = Some("2024-08-22"),
    alert = "2023-08-31",
    occludes = Set(SatDinner)
  )

  Show(
    "BackToSchool2024i",
    StandardPromo,
    str("WTUL is hosting a Back 2 School Bash on Thursday the 22nd at The Broadside! Doors at 5, show at 6. This funky lineup is loaded with tunes that will make you feel too cool for school, from Lady Li, Doctors, The Dewdrops, Raph and the Rotation and Planet of the Little Green Men! There's more information on our website and social media pages!"),
    start = "2024-08-18",
    end = Some("2024-08-22"),
    alert = "2023-08-31",
    occludes = Set(SatDinner)
  )

  Show(
    "BackToSchool2024j",
    StandardPromo,
    str("WTUL is hosting a Back 2 School Bash on Thursday the 22nd at The Broadside! Doors at 5, show at 6. This funky lineup is loaded with tunes that will make you feel too cool for school, from Lady Li, Doctors, The Dewdrops, Raph and the Rotation and Planet of the Little Green Men! There's more information on our website and social media pages!"),
    start = "2024-08-18",
    end = Some("2024-08-22"),
    alert = "2023-08-31",
    occludes = Set(SatDinner)
  )

  Event("BackToSchool2024k",
    str("WTUL is hosting a Back 2 School Bash") +
      blank("when") +
      str("at The Broadside! Doors at 5, show at 6. This funky lineup is loaded with tunes that will make you feel too cool for school, from Lady Li, Doctors, The Dewdrops, Raph and the Rotation and Planet of the Little Green Men! There's more information on our website and social media pages!"),
    "2024-08-22"
  )

  Show(
    "BackToSchool2024l",
    StandardPromo,
    str("WTUL is hosting a Back 2 School Bash on Thursday the 22nd at The Broadside! Doors at 5, show at 6. This funky lineup is loaded with tunes that will make you feel too cool for school, from Lady Li, Doctors, The Dewdrops, Raph and the Rotation and Planet of the Little Green Men! There's more information on our website and social media pages!"),
    start = "2024-08-18",
    end = Some("2024-08-22"),
    alert = "2023-08-31",
    occludes = Set(SatDinner)
  )

  Show(
    "BackToSchool2024m",
    StandardPromo,
    str("WTUL is hosting a Back 2 School Bash on Thursday the 22nd at The Broadside! Doors at 5, show at 6. This funky lineup is loaded with tunes that will make you feel too cool for school, from Lady Li, Doctors, The Dewdrops, Raph and the Rotation and Planet of the Little Green Men! There's more information on our website and social media pages!"),
    start = "2024-08-18",
    end = Some("2024-08-22"),
    alert = "2023-08-31",
    occludes = Set(SatDinner)
  )

  Show(
    "BackToSchool2024n",
    StandardPromo,
    str("WTUL is hosting a Back 2 School Bash on Thursday the 22nd at The Broadside! Doors at 5, show at 6. This funky lineup is loaded with tunes that will make you feel too cool for school, from Lady Li, Doctors, The Dewdrops, Raph and the Rotation and Planet of the Little Green Men! There's more information on our website and social media pages!"),
    start = "2024-08-18",
    end = Some("2024-08-22"),
    alert = "2023-08-31",
    occludes = Set(SatDinner)
  )

  Show(
    "BackToSchool2024p",
    StandardPromo,
    str("WTUL is hosting a Back 2 School Bash on Thursday the 22nd at The Broadside! Doors at 5, show at 6. This funky lineup is loaded with tunes that will make you feel too cool for school, from Lady Li, Doctors, The Dewdrops, Raph and the Rotation and Planet of the Little Green Men! There's more information on our website and social media pages!"),
    start = "2024-08-18",
    end = Some("2024-08-22"),
    alert = "2023-08-31",
    occludes = Set(SatDinner)
  )

  // After back-to-school 2024

  Show(
    "AltOldies3",
    StandardPromo,
    str(
      "Tune in to the Alt Oldies show every Saturday night on WTUL from 8 until 10 to hear music left of the radio dial as it was in past decades.  Let the DJs take you on a sonic joy ride through the past that includes old prog, vintage new wave and punk, underground rock and soul, synth wave, psychedelia and much more.  If it’s vintage and cool, the DJs will blow off the dust and play it for ya!  That's every Saturday from 8 until 10")
      > pm + str("only on WTUL New Orleans."),
    start = "2024-08-23",
    alert = "2023-08-31",
    occludes = Set(SatDinner)
  )

  Show(
    "LocalShow3",
    StandardPromo,
    str(
      "The Local show is a blend of not-so-secret herbs and spices, the holy trinity of creole cooking blended with local radio. Lettuce turnip the beets to the sweet and spicy sounds of local artists in New Orleans and Louisiana. Sometimes DJ Chef stirs the pot, other times DJ Lank takes the ladle. No matter what, it's a flavor-filled guaranteed good-taste-of-a-time. Fill up on the local show, Tuesdays 8-10") > pm + str("on WTUL New Orleans, 91.5 FM."),
    start = "2024-08-23",
    end = Some("2025-11-22"),
    alert = "2023-08-31",
    occludes = Set(TueAfternoon, TueDinner)
  )

  Show(
    "JamBand4",
    StandardPromo,
    str(
      "Check out the Friday Night Jam Session show, every other Friday from 8 to 10")
      > pm > period
      + str("Host DJ Uptown Ruler explores jam bands from the 90s to the present.  Journey through the night with a good dose of eclectic, contemporary psychedelic rock music featuring extended jams.  Jam out right here with WTUL every other Friday night."),
    start = "2024-08-23",
    alert = "2023-08-31",
    occludes = Set(FriAfternoon, FriDinner)
  )

  Spot(
    "SociaMedia3",
    StandardPromo,
    str("If you want to keep in touch with WTUL's events, playlists, and other updates, check out our social media pages! Our handle on Twitter and Instagram is")
      + online("@WTUL") > comma
      + str("or search for our Facebook page under")
      + online("WTUL New Orleans") > period
      + str("And you can find our website at")
      + online("W T U L dot F M")
      > period,
    start = "2024-08-23",
    alert = "2022-12-05"
  )

  Spot(
    "PSAcall3",
    StandardPromo,
    str("Does your group work for the good of New Orleans?  WTUL's public service announcements prioritize charitable activities, human services, and volunteer opportunities.  We also announce public meetings, grassroots organizations, local issue-oriented community activities, and free public lectures and exhibits.")
      + moreWebEmail("W T U L new orleans dot com slash about slash P S A S", "W T U L hyphen P S A at gmail dot com"),
    start = "2024-08-23",
    alert = "2022-12-05"
  )

  Show(
    "DemocracyNow3",
    StandardPromo,
    it("Democracy Now")
      + str("airs Mondays, Wednesdays, and Fridays at 9") > am
      + str("right here on WTUL New Orleans.")
      + it("Democracy Now") > comma
      + str("with host Amy Goodman, offers award-winning journalism and fresh and progressive perspectives on national and international issues."),
    start = "2024-08-23",
    alert = "2022-12-05",
    occludes = Set(MonMorning, WedMorning, FriMorning)
  )

  Show(
    "Jazz3",
    StandardPromo,
    str("Every weekday evening, tune your radios to 91.5FM for sweet jazz music from artists new and old, local and global.  You don't want to miss it!  Listen to the World of Jazz every weeknight, Monday through Friday, from 6 to 8")
      > pm
      + str("on WTUL New Orleans."),
    start = "2024-08-23",
    alert = "2022-12-05",
    occludes = Set(MonDinner, TueDinner, WedDinner, ThuDinner, FriDinner)
  )

  Show(
    "Surf3",
    StandardPromo,
    str("Storm Surge of Reverb serves up a jumbo portion of high-energy surf and instrumental rock-and-roll music every Monday from 4 to 6")
      > pm > period
      + str("It's not just Wipe Out on repeat, it's vintage and new bands from around the world, playing wild, strange and wonderful rock-and-roll instrumentals. That's Storm Surge of Reverb, every Monday from 4 to 6")
      > pm > period,
    start = "2024-08-23",
    alert = "2022-12-05",
    occludes = Set(MonAfternoon)
  )

  Show(
    "Country3",
    StandardPromo,
    str("You can catch WTUL's Country Show every Sunday from 2 to 4")
      > pm > period
      + str("Tune in for two hours of folk, blues, cowpunk, western swing, that rockabilly thing, outlaw and everything in between, right here on WTUL."),
    start = "2024-08-23",
    alert = "2022-12-05",
    occludes = Set(SunAfternoon)
  )

  Show(
    "TwentyTwentyOne3",
    StandardPromo,
    str("Sunday Nights at 8")
      > pm > comma
      + str("WTUL's classical and experimental program ")
      + phonetic(str("20/21"), "Twenty Twenty-One")
      + str("rounds out the weekend with four hours of music from the birth of modernism to today. Each week, DJ Pennebaker explores a range of styles and sounds --- from contemporary compositions to 20th century classics --- with excursions into electronic music and the avant-garde.  A Sunday night feature for nearly forty years, 20/21 airs Sunday from 8") > pm + str("to midnight here on WTUL."),
    start = "2024-08-23",
    alert = "2022-12-05",
    occludes = Set(SunNight)
  )

  Show(
    "Cheez3",
    StandardPromo,
    str("Tune in to WTUL every Sunday from 6 to 10") > am
      + str("for the Cheez Muzik Show.  What is Cheez, you may ask? Space Music, Ambient, Berlin School, Downtempo, New Age. This music is cheesy! Tune into the Cheez Muzik Show every Sunday morning from 6 to 10")
      > am > period,
    start = "2024-08-23",
    alert = "2022-12-05",
    occludes = Set()
  )

  Show(
    "FolkAmericana3",
    StandardPromo,
    str("Every Saturday afternoon, WTUL brings you four hours of Americana music. At noon, the folk show with host Mark T brings you singer-songwriters, fiddlers, bluegrass, and more. The folk show is followed by the Americana show at 2")
      > pm > comma
      + str("dusting off the old and bringing you new Americana, rockabilly, and alt-country music. Tune in every Saturday from noon to four for your weekly dose of down-home roots music right here on WTUL New Orleans."),
    start = "2024-08-23",
    alert = "2022-12-05",
    occludes = Set(SatAfternoon)
  )

  Show(
    "Kids3",
    StandardPromo,
    str("Every Saturday morning, the Kid's Show with DJ Liz E brings you family-friendly programming to start your day off right. Join us for music and stories for kids of all ages. It's a pajama dance party right in your living room, and it's only here on WTUL, Saturdays from 8 to 10") > am > period,
    start = "2024-08-23",
    alert = "2999-12-31",
    occludes = Set(SatMorning)
  )

  // Before back-to-school 2024

  Show(
    "AltOldies2",
    StandardPromo,
    str(
      "Tune in to the Alt Oldies show every Saturday night on WTUL from 8 until 10 to hear music left of the radio dial as it was in past decades.  Let the DJs take you on a sonic joy ride through the past that includes old prog, vintage new wave and punk, underground rock and soul, synth wave, psychedelia and much more.  If it’s vintage and cool, the DJs will blow off the dust and play it for ya!  That's every Saturday from 8 until 10")
      > pm + str("only on WTUL New Orleans."),
    start = "2024-04-15",
    end = Some("2024-08-17"),
    alert = "2023-08-31",
    occludes = Set(SatDinner)
  )

  Show(
    "LocalShow2",
    StandardPromo,
    str(
      "The Local show is a blend of not-so-secret herbs and spices, the holy trinity of creole cooking blended with local radio. Lettuce turnip the beets to the sweet and spicy sounds of local artists in New Orleans and Louisiana. Sometimes DJ Chef stirs the pot, other times DJ Lank takes the ladle. No matter what, it's a flavor-filled guaranteed good-taste-of-a-time. Fill up on the local show, Tuesdays 8-10") > pm + str("on WTUL New Orleans, 91.5 FM."),
    start = "2024-04-15",
    end = Some("2024-08-17"),
    alert = "2023-08-31",
    occludes = Set(TueAfternoon, TueDinner)
  )

  Show(
    "JamBand3",
    StandardPromo,
    str(
      "Check out the Friday Night Jam Session show, every other Friday from 8 to 10")
      > pm > period
      + str("Host DJ Uptown Ruler explores jam bands from the 90s to the present.  Journey through the night with a good dose of eclectic, contemporary psychedelic rock music featuring extended jams.  Jam out right here with WTUL every other Friday night."),
    start = "2024-04-27",
    end = Some("2024-08-17"),
    alert = "2023-08-31",
    occludes = Set(FriAfternoon, FriDinner)
  )

  Show(
    "JamBand2",
    StandardPromo,
    str(
      "Check out the Friday Night Jam Session show, every Friday from 8 to 10")
      > pm > period
      + str("Host DJ Uptown Ruler explores jam bands from the 90s to the present.  Journey through the night with a good dose of eclectic, contemporary psychedelic rock music featuring extended jams.  Jam out right here with WTUL on Friday nights."),
    start = "2024-04-15",
    end = Some("2024-04-26"),
    alert = "2023-08-31",
    occludes = Set(FriAfternoon, FriDinner)
  )

  Spot(
    "SociaMedia2",
    StandardPromo,
    str("If you want to keep in touch with WTUL's events, playlists, and other updates, check out our social media pages! Our handle on Twitter and Instagram is")
      + online("@WTUL") > comma
      + str("or search for our Facebook page under")
      + online("WTUL New Orleans") > period
      + str("And you can find our website at")
      + online("W T U L dot F M")
      > period,
    start = "2024-04-15",
    end = Some("2024-08-17"),
    alert = "2022-12-05"
  )

  Spot(
    "PSAcall2",
    StandardPromo,
    str("Does your group work for the good of New Orleans?  WTUL's public service announcements prioritize charitable activities, human services, and volunteer opportunities.  We also announce public meetings, grassroots organizations, local issue-oriented community activities, and free public lectures and exhibits.")
      + moreWebEmail("W T U L new orleans dot com slash about slash P S A S", "W T U L hyphen P S A at gmail dot com"),
    start = "2024-04-15",
    end = Some("2024-08-17"),
    alert = "2022-12-05"
  )

  Show(
    "DemocracyNow2",
    StandardPromo,
    it("Democracy Now")
      + str("airs Mondays, Wednesdays, and Fridays at 9") > am
      + str("right here on WTUL New Orleans.")
      + it("Democracy Now") > comma
      + str("with host Amy Goodman, offers award-winning journalism and fresh and progressive perspectives on national and international issues."),
    start = "2024-04-15",
    end = Some("2024-08-17"),
    alert = "2022-12-05",
    occludes = Set(MonMorning, WedMorning, FriMorning)
  )

  Show(
    "Jazz2",
    StandardPromo,
    str("Every weekday evening, tune your radios to 91.5FM for sweet jazz music from artists new and old, local and global.  You don't want to miss it!  Listen to the World of Jazz every weeknight, Monday through Friday, from 6 to 8")
      > pm
      + str("on WTUL New Orleans."),
    start = "2024-04-15",
    end = Some("2024-08-17"),
    alert = "2022-12-05",
    occludes = Set(MonDinner, TueDinner, WedDinner, ThuDinner, FriDinner)
  )

  Show(
    "Surf2",
    StandardPromo,
    str("Storm Surge of Reverb serves up a jumbo portion of high-energy surf and instrumental rock-and-roll music every Monday from 4 to 6")
      > pm > period
      + str("It's not just Wipe Out on repeat, it's vintage and new bands from around the world, playing wild, strange and wonderful rock-and-roll instrumentals. That's Storm Surge of Reverb, every Monday from 4 to 6")
      > pm > period,
    start = "2024-04-15",
    end = Some("2024-08-17"),
    alert = "2022-12-05",
    occludes = Set(MonAfternoon)
  )

  Show(
    "Country2",
    StandardPromo,
    str("You can catch WTUL's Country Show every Sunday from 2 to 4")
      > pm > period
      + str("Tune in for two hours of folk, blues, cowpunk, western swing, that rockabilly thing, outlaw and everything in between, right here on WTUL."),
    start = "2024-04-15",
    end = Some("2024-08-17"),
    alert = "2022-12-05",
    occludes = Set(SunAfternoon)
  )

  Show(
    "TwentyTwentyOne2",
    StandardPromo,
    str("Sunday Nights at 8")
      > pm > comma
      + str("WTUL's classical and experimental program ")
      + phonetic(str("20/21"), "Twenty Twenty-One")
      + str("rounds out the weekend with four hours of music from the birth of modernism to today. Each week, DJ Pennebaker explores a range of styles and sounds --- from contemporary compositions to 20th century classics --- with excursions into electronic music and the avant-garde.  A Sunday night feature for nearly forty years, 20/21 airs Sunday from 8") > pm + str("to midnight here on WTUL."),
    start = "2024-04-15",
    end = Some("2024-08-17"),
    alert = "2022-12-05",
    occludes = Set(SunNight)
  )

  Show(
    "Cheez2",
    StandardPromo,
    str("Tune in to WTUL every Sunday from 6 to 10") > am
      + str("for the Cheez Muzik Show.  What is Cheez, you may ask? Space Music, Ambient, Berlin School, Downtempo, New Age. This music is cheesy! Tune into the Cheez Muzik Show every Sunday morning from 6 to 10")
      > am > period,
    start = "2024-04-15",
    end = Some("2024-08-17"),
    alert = "2022-12-05",
    occludes = Set()
  )

  Show(
    "FolkAmericana2",
    StandardPromo,
    str("Every Saturday afternoon, WTUL brings you four hours of Americana music. At noon, the folk show with host Mark T brings you singer-songwriters, fiddlers, bluegrass, and more. The folk show is followed by the Americana show at 2")
      > pm > comma
      + str("dusting off the old and bringing you new Americana, rockabilly, and alt-country music. Tune in every Saturday from noon to four for your weekly dose of down-home roots music right here on WTUL New Orleans."),
    start = "2024-04-15",
    end = Some("2024-08-17"),
    alert = "2022-12-05",
    occludes = Set(SatAfternoon)
  )

  Show(
    "Kids2",
    StandardPromo,
    str("Every Saturday morning, the Kid's Show with DJ Liz E brings you family-friendly programming to start your day off right. Join us for music and stories for kids of all ages. It's a pajama dance party right in your living room, and it's only here on WTUL, Saturdays from 8 to 10") > am > period,
    start = "2024-04-15",
    end = Some("2024-08-17"),
    alert = "2999-12-31",
    occludes = Set(SatMorning)
  )

  // -----------------------------------------------------------------

  Show(
    "AltOldies",
    StandardPromo,
    str(
      "Tune in to the Alt Oldies show every Saturday night on WTUL from 8 until 10 to hear music left of the radio dial as it was in past decades.  Let the DJs take you on a sonic joy ride through the past that includes old prog, vintage new wave and punk, underground rock and soul, synth wave, psychedelia and much more.  If it’s vintage and cool, the DJs will blow off the dust and play it for ya!  That's every Saturday from 8 until 10")
      > pm + str("only on WTUL New Orleans."),
    start = "2023-07-03",
    end = Some("2024-03-27"),
    alert = "2023-08-31",
    occludes = Set(SatDinner)
  )

  Show(
    "LocalShow",
    StandardPromo,
    str(
      "The Local show is a blend of not-so-secret herbs and spices, the holy trinity of creole cooking blended with local radio. Lettuce turnip the beets to the sweet and spicy sounds of local artists in New Orleans and Louisiana. Sometimes DJ Chef stirs the pot, other times DJ Lank takes the ladle. No matter what, it's a flavor-filled guaranteed good-taste-of-a-time. Fill up on the local show, Tuesdays 8-10") > pm + str("on WTUL New Orleans, 91.5 FM."),
    start = "2023-07-03",
    end = Some("2024-03-27"),
    alert = "2023-08-31",
    occludes = Set(TueAfternoon, TueDinner)
  )

  Show(
    "JamBand",
    StandardPromo,
    str(
      "Check out the Friday Night Jam Session show, every Friday from 8 to 10")
      > pm > period
      + str("Host DJ Uptown Ruler explores jam bands from the 90s to the present.  Journey through the night with a good dose of eclectic, contemporary psychedelic rock music featuring extended jams.  Jam out right here with WTUL on Friday nights."),
    start = "2023-07-03",
    end = Some("2024-03-27"),
    alert = "2023-08-31",
    occludes = Set(FriAfternoon, FriDinner)
  )

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
    end = Some("2024-03-27"),
    alert = "2022-12-05"
  )

  Spot(
    "PSAcall",
    StandardPromo,
    str("Does your group work for the good of New Orleans?  WTUL's public service announcements prioritize charitable activities, human services, and volunteer opportunities.  We also announce public meetings, grassroots organizations, local issue-oriented community activities, and free public lectures and exhibits.")
      + moreWebEmail("W T U L new orleans dot com slash about slash P S A S", "W T U L hyphen P S A at gmail dot com"),
    start = "2022-09-12",
    end = Some("2024-03-27"),
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
    end = Some("2024-03-27"),
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
    end = Some("2024-03-27"),
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
    end = Some("2024-03-27"),
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
    end = Some("2024-03-27"),
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
    end = Some("2024-03-27"),
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
    end = Some("2024-03-27"),
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
    end = Some("2024-03-27"),
    alert = "2022-12-05",
    occludes = Set(SatAfternoon)
  )

  Show(
    "Kids",
    StandardPromo,
    str("Every Saturday morning, the Kid's Show with DJ Liz E brings you family-friendly programming to start your day off right. Join us for music and stories for kids of all ages. It's a pajama dance party right in your living room, and it's only here on WTUL, Saturdays from 8 to 10") > am > period,
    start = "2000-01-01",
    end = Some("2024-03-27"),
    alert = "2999-12-31",
    occludes = Set(SatMorning)
  )

  // -----------------------------------------------------------------


  Show(
    "Antenna2024",
    StandardPromo,
    str(
      "If you’ve been having a hard time catching WTUL on your stereo, fear not -- our transmission troubles are coming to an end! Our new antenna is now on top of Tidewater Building on Canal Street and it’s going to be sending out crystal clear radio goodness across New Orleans once more. Tulane University and your donations have made this happen so thank you! For more info, read the antenna story at")
      + online("W T U L new orleans dot com")
      > period,
    start = "2024-03-28",
    end = Some("2024-04-12"),
    alert = "2024-04-15",
    occludes = Set()
  )

  Show(
    "Marathon2024BrassShow",
    StandardPromo,
    str("It’s Marathon time! Check out WTUL’s  ‘TULbox Show at Tipitina’s featuring Brass-A-Holics and Brass Hearts on Wednesday the third.  The show’s at 8 and tickets are available at")
      + online("tipitinas dot com")
      + str("or at the door."),
    start = "2024-03-28",
    end = Some("2024-04-03"),
    alert = "2024-04-07",
    occludes = Set()
  )

  Show(
    "Marathon2024PunkShow",
    StandardPromo,
    str("At 8") > pm
      + str("on Friday, April 5th, WTUL is hosting a Punk Show at The Goat with Sesher, BS Machine and Code Black. Come see these great bands and support WTUL!"),
    start = "2024-03-28",
    end = Some("2024-04-05"),
    alert = "2024-04-05",
    occludes = Set()
  )

  Show(
    "Marathon2024Alumni",
    StandardPromo,
    str("The WTUL Alumni Reunion is Sunday, April 7th at Cooter Brown’s at four") > pm > period
      + str("There’ll be food, good music, old friends and great stories! From the station’s earliest days to now, relive your best (and maybe worst) moments at WTUL!"),
    start = "2024-03-28",
    end = Some("2024-04-07"),
    alert = "2024-04-08",
    occludes = Set()
  )

  Show(
    "Marathon2024Trivia",
    StandardPromo,
    str("Test your music knowledge with Euclid Records Owner Lefty Parker, Journalist Alison Fensterstock and Ziggy the Wonder Dog. WTUL is hosting Music Trivia at 6:30") > pm
      + str("on Monday April 8th at Dat Dog Freret. Compete solo or with a team and win cool prizes while you support WTUL!"),
    start = "2024-03-28",
    end = Some("2024-04-08"),
    alert = "2024-04-09",
    occludes = Set()
  )

  Show(
    "Marathon2024BattleOfTheBands",
    StandardPromo,
    str("Test your music knowledge with Euclid Records Owner Lefty Parker, Journalist Alison Fensterstock and Ziggy the Wonder Dog. WTUL is hosting Music Trivia at 6:30") > pm
      + str("on Monday April 8th at Dat Dog Freret. Compete solo or with a team and win cool prizes while you support WTUL!"),
    start = "2024-03-28",
    end = Some("2024-04-08"),
    alert = "2024-04-09",
    occludes = Set()
  )

  Show(
    "Marathon2024Main",
    StandardPromo,
    str("The main Marathon event starts at noon, Friday April 12th as DJ Fallah Steen takes the first shift during 24-Hour DJ Weekend. DJ Skitch takes over the airwaves at noon Saturday, followed by Laura B at noon Sunday. Listen to these amazing DJs as they fight the sleepies to bring you 24 hours each of outstanding interviews, live music and WTUL tunes."),
    start = "2024-03-28",
    end = Some("2024-04-12"),
    alert = "2024-04-13",
    occludes = Set()
  )

  Show(
    "Marathon2024KidsShow",
    StandardPromo,
    str("The Kids Show will be live Saturday, April 13th on the LBC Quad! Join DJ LizE from 8-10") > am
      + str("for a live performance by musician Johnette Downing, lots of bubbles and crafts, story time, snacks, and great music. Kids of all ages are welcome!"),
    start = "2024-03-28",
    end = Some("2024-04-08"),
    alert = "2024-04-09",
    occludes = Set()
  )

  // -----------------------------------------------------------------

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
