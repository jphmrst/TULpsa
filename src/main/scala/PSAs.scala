// PSAs.scala --- (c) 2021 John Maraist
// Part of the WTUL Roster Generator
//
// This file is made available under the GNU GPL version 3; see the
// LICENSE file in this distribution or https://fsf.org/ for more
// details.

package org.maraist.wtulrosters
import java.time.LocalDate
import java.time.format.DateTimeFormatter

object PSAutils {
  def basePolicy(slot: Int): Int =
    if slot < 14 then 3 else
      if slot < 29 then 5 else
        if slot < 46 then 3
          else 5
}

/** How we generate PSA rosters. */
object PsaRosters extends RosterType {

  override def init(): Unit = {
    PsaShortTermSpots.init()
    PsaLongTermSpots.init()
    PsaScheduling.init()
  }

  override protected type RBuilder = PsaRosterBuilder

  override protected
  def newBuilder(date: LocalDate): PsaRosterBuilder =
    new PsaRosterBuilder(date)

  override protected
  def complete(builder: PsaRosterBuilder): Unit = {
    builder.fillByDayMatch(PsaShortTermSpots)
    builder.fillByAssortment(PsaLongTermSpots, PsaScheduling)
  }
}

/** Specialization of [[RosterBuilder]] with the specifics for PSAs.
  * @param startDate The first date of the week to be covered by this
  * roster.
  */
class PsaRosterBuilder(startDate: LocalDate)
    extends RosterBuilder(startDate, 78, PsaScheduling,
      "WTUL 91.5\\textsc{fm} --- PSA roster",
      "PSA \\#",
      "Please report typos, expired spots, or other problems with PSAs to \\textsl{wtul-psa@gmail.com}\\,.",
      (x: Int) => (1 + x).toString(),
      commonPreamble,
      DateTimeFormatter.ofPattern("MMMM d, yyyy, h:mm'{\\small 'a'}'"),
      "PSA-",
      (first, last) => {
        val base = PSAutils.basePolicy(first)
        val avail = last - first + 1
        if avail <= base + 1
        then avail
        else if avail >= 2 * base - 1
        then base
        else avail / 2
      },
      Array[Int | List[Int]](
        0, 1, 2, 3, 4, 5, 6, // Aa  0-6
        0, 1, 2, 3, 4, 5, 6, // Ab  7-13
        0, 1, 2, 3, 4, 5, 6, // Ba  14-20
        0, 1, 2, 3, 4, 5, 6, // Bb  21-27
        0, List(1, 2), List(2, 3), 4, List(5, 6),          // Ca  28-32
        List(0, 1), List(1, 2), 3, List(4, 5), List(5, 6), // Cb  33-37
        List(0, 1), List(2, 3), List(4, 5), 6,  // Da  38-41
        List(0, 1), 2, List(3, 4), List(5, 6),  // Db  42-45

        List(0, 1), List(2, 3), List(4, 5), // Ea  46-48
        List(0, 1), List(2, 3), List(4, 6), // Eb  49-51
        List(0, 1), List(2, 3), List(5, 6), // Ec  52-54
        List(0, 1), List(2, 4), List(5, 6), // Ed  55-57
        List(0, 1), List(3, 4), List(5, 6), // Ee  58-60
        List(0, 2), List(3, 4), List(5, 6), // Ef  61-63
        List(1, 2), List(3, 4), List(5, 6), // Eg  64-66
        List(0, 1), List(2, 3), List(4, 5), // Eh  67-69
        List(0, 1), List(2, 3), List(4, 6), // Ei  70-72
        List(0, 1), List(2, 3), List(5, 6), // Ej  73-75
        List(4, 6), // Fa  76
        List(5, 6), // Fb  77
      )
) {
  val slotOrders: List[List[Int]] = List(
    List(
      0, 1, 2, 3, 4, 5, 6,
      7, 8, 9, 10, 11, 12, 13,
      14, 15, 16, 17, 18, 19, 20,
      21, 22, 23, 24, 25, 26, 27,
      28, 29,
      30, 31, 32, 33, 34, 35, 36, 37, 38, 39,
      40, 41, 42, 43, 44, 45, 46, 47, 48, 49,
      50, 51, 52, 53, 54, 55, 56, 57, 58, 59,
      60, 61, 62, 63, 64, 65, 66, 67, 68, 69,
      70, 71, 72, 73, 74, 75, 76, 77),
    List(
      7, 8, 9, 10, 11, 12, 13,
      14, 15, 16, 17, 18, 19, 20,
      21, 22, 23, 24, 25, 26, 27,
      0, 1, 2, 3, 4, 5, 6,
      28, 29,
      30, 31, 32, 33, 34, 35, 36, 37, 38, 39,
      40, 41, 42, 43, 44, 45, 46, 47, 48, 49,
      50, 51, 52, 53, 54, 55, 56, 57, 58, 59,
      60, 61, 62, 63, 64, 65, 66, 67, 68, 69,
      70, 71, 72, 73, 74, 75, 76, 77),
    List(
      14, 15, 16, 17, 18, 19, 20,
      21, 22, 23, 24, 25, 26, 27,
      0, 1, 2, 3, 4, 5, 6,
      7, 8, 9, 10, 11, 12, 13,
      28, 29,
      30, 31, 32, 33, 34, 35, 36, 37, 38, 39,
      40, 41, 42, 43, 44, 45, 46, 47, 48, 49,
      50, 51, 52, 53, 54, 55, 56, 57, 58, 59,
      60, 61, 62, 63, 64, 65, 66, 67, 68, 69,
      70, 71, 72, 73, 74, 75, 76, 77),
    List(
      21, 22, 23, 24, 25, 26, 27,
      0, 1, 2, 3, 4, 5, 6,
      7, 8, 9, 10, 11, 12, 13,
      14, 15, 16, 17, 18, 19, 20,
      28, 29,
      30, 31, 32, 33, 34, 35, 36, 37, 38, 39,
      40, 41, 42, 43, 44, 45, 46, 47, 48, 49,
      50, 51, 52, 53, 54, 55, 56, 57, 58, 59,
      60, 61, 62, 63, 64, 65, 66, 67, 68, 69,
      70, 71, 72, 73, 74, 75, 76, 77),
    List(
      6, 5, 4, 3, 2, 1, 0,
      27, 26, 25, 24, 23, 22, 21,
      20, 19, 18, 17, 16, 15, 14,
      13, 12, 11, 10, 9, 8, 7,
      28, 29,
      30, 31, 32, 33, 34, 35, 36, 37, 38, 39,
      40, 41, 42, 43, 44, 45, 46, 47, 48, 49,
      50, 51, 52, 53, 54, 55, 56, 57, 58, 59,
      60, 61, 62, 63, 64, 65, 66, 67, 68, 69,
      70, 71, 72, 73, 74, 75, 76, 77),
    List(
      13, 12, 11, 10, 9, 8, 7,
      6, 5, 4, 3, 2, 1, 0,
      27, 26, 25, 24, 23, 22, 21,
      20, 19, 18, 17, 16, 15, 14,
      28, 29,
      30, 31, 32, 33, 34, 35, 36, 37, 38, 39,
      40, 41, 42, 43, 44, 45, 46, 47, 48, 49,
      50, 51, 52, 53, 54, 55, 56, 57, 58, 59,
      60, 61, 62, 63, 64, 65, 66, 67, 68, 69,
      70, 71, 72, 73, 74, 75, 76, 77),
    List(
      20, 19, 18, 17, 16, 15, 14,
      13, 12, 11, 10, 9, 8, 7,
      6, 5, 4, 3, 2, 1, 0,
      27, 26, 25, 24, 23, 22, 21,
      28, 29,
      30, 31, 32, 33, 34, 35, 36, 37, 38, 39,
      40, 41, 42, 43, 44, 45, 46, 47, 48, 49,
      50, 51, 52, 53, 54, 55, 56, 57, 58, 59,
      60, 61, 62, 63, 64, 65, 66, 67, 68, 69,
      70, 71, 72, 73, 74, 75, 76, 77),
    List(
      27, 26, 25, 24, 23, 22, 21,
      20, 19, 18, 17, 16, 15, 14,
      13, 12, 11, 10, 9, 8, 7,
      6, 5, 4, 3, 2, 1, 0,
      28, 29,
      30, 31, 32, 33, 34, 35, 36, 37, 38, 39,
      40, 41, 42, 43, 44, 45, 46, 47, 48, 49,
      50, 51, 52, 53, 54, 55, 56, 57, 58, 59,
      60, 61, 62, 63, 64, 65, 66, 67, 68, 69,
      70, 71, 72, 73, 74, 75, 76, 77)
  )

  override def slotOrder: List[Int] =
    import Spot.getWeekOfCentury
    slotOrders(startDate.getWeekOfCentury() % slotOrders.length)
}

/** Bank holding short-term PSAs. */
object PsaShortTermSpots extends SpotBank("psa-short", PsaScheduling) {
  import Group.*
  import scala.language.implicitConversions

  Event("LionsScreeningsLakesideAug2021",
    """The Lions Clubs of Louisiana support eye and ear health for children.  They will conduct their annual ``Lions  Health Awareness Day'' program of vision screening and other services on %%when.%%  Screenings will provide recommendations for a doctor's visit when necessary.  There is no charge for these screenings.   The screenings will be from 11\\AM\\ to 3\\PM\\ in the center court of the Lakeside Shopping Center.""",
    "2021-08-28",
    spotsSourceContacts = Seq("Aida Grace <dajg@aol.com>")
  )

  Event("sierraMtgJuly21",
    """The next meeting of the Orleans Sierra Club will be %%when%% at 6:30\PM.  Christen Steele will discuss conservation issues affecting the survival of the monarch butterfly.  Sierra Club meetings are currently online.  More information and a link to the online session are available on their website, \online{sierra club dot O R G slash delta}.""",
    "2021-07-21",
    spotsSourceURL = Seq("https://www.sierraclub.org/delta/new-orleans-group")
  )
}

/** Bank holding long-term PSAs. */
object PsaLongTermSpots extends SpotBank("psa-long", PsaScheduling) {
  import Group.*
  import scala.language.implicitConversions

  val voteDotOrg = Spot(
    "VoteDotOrg",
    Volunteer,
    "Do you need to register to vote, check your registration status, or find your polling place?  Do you want to volunteer to be an election site poll worker?  You can find out more about voter and election information at \\online{vote dot O R G}. Quick links will connect you to every state! For your state of residence, you can register, check registration status, find your polling place, request an absentee ballot, or volunteer to be a poll worker. More information is available at \\online{vote dot O R G}.",
    start = "2021-06-22",
    alert = "2021-07-23",
    sourceNote = "Local"
  )

  val hnoc = Spot(
    "HnocVolunteersTwelve",
    Volunteer,
    "The Historic New Orleans Collection is a museum, research center, and publisher in the French Quarter. They are looking for weekend volunteers to help greet visitors, monitor exhibitions, and lead tours. \\MoreWeb{H N O C dot O R G}",
    start = "2021-06-22",
    alert = "2021-07-23",
    sourceNote = "contact email from their web site, wrc@hnoc.org"
  )

  Spot(
    "SharedHousingOfNOLATwo",
    Volunteer,
    "Shared Housing of New Orleans matches people seeking places to live with elderly and disabled homeowners.  The home-seeker does light housekeeping, and stays on the premises at night. The home owner avoids nursing home or other assisted-living facilities by having this assistance and companionship.  \\MoreWebPhone{shared housing of new orleans dot org}{504/896-2575}",
    start = "2021-06-22",
    alert = "2022-07-15",
    sourceNote = "confirmed July 2015 --- strauss@sharedhousingofneworleans.org (Marion Strauss)"
  )

  Spot(
    "InnocenceProjectGeneralTwo",
    Volunteer,
    "Innocence Project New Orleans represents prisoners serving life sentences in Louisiana and Mississippi. They work to free innocent prisoners, prevent wrongful convictions, and assist freed prisoners with their transition upon release.   \\MorePhoneWeb[Information about supporting the Innocence Project is]{504/522-4766}{I P hyphen N O dot org}",
    start = "2021-06-22",
    alert = "2022-07-15",
    sourceContacts = Seq("info@ip-no.org", "Jene OKeefe Trigg <JeneOT@ip-no.org>"),
    sourceNote = "confirmed August 2015"
  )

  Spot(
    "JeffParishAdoptParkwayTwo",
    Volunteer,
    "Jefferson Parish's Adopt-a-Parkway Program started in 1988. Parkway sponsors improve landscape of their adopted sections of thoroughfares, and are recognized by a sign on their neutral ground.  More information is available on the Jefferson Parish website \\online{jeff parish dot net}, or by phone at {504/349-5829}.",
    start = "2021-06-22",
    alert = "2022-07-15",
    sourceNote = "updated from their website July 2015, added 9/2009"
  )

  Spot(
    "HabitatStTammanyReStore",
    Volunteer,
    "Habitat for Humanity can make use of used housewares or surplus building materials.  Donated materials are made available for purchase at the Habitat for Humanity Re-Store, located on North Lane, off Highway 59, just north of I-12. Proceeds benefit Habitat for Humanity.  \\MorePhone[More information including directions and volunteering is]{985/898-0642}",
    start = "2021-06-22",
    alert = "2022-07-15",
    sourceNote = "## checked details online July 2015"
  )

  Spot(
    "CathCharitiesReadToKidsTwo",
    Volunteer,
    "Catholic Charities seeks volunteers to read or tutor children and youth, work with seniors, and perform other duties. \\MorePhoneWeb{504/310-6960}{C C A N O dot org}",
    start = "2021-06-22",
    alert = "2022-07-15",
    sourceNote = "Pulled from Gambit, checked details online July 2015"
  )

  Spot(
    "ClearwaterWildlifeSanctuaryB",
    Volunteer,
    "The Clearwater Wildlife Sanctuary seeks volunteers fostering abandoned young and rehabilitating injured animals and birds. Help is also needed at the Covington hospital site, the Madisonville educational center, and with transportating creatures. They also offer classes, including for skilled animal handling volunteers.  \\MoreWebPhone{clear water sanctuary dot O R G}{985/630-1009}",
    start = "2021-06-22",
    alert = "2022-07-15",
    sourceContacts = Seq("clearwaterwildlife@gmail.com"),
    sourceNote = "## updated Feb. 2016"
  )

  Spot(
    "CommInSchoolsNolaGeneral",
    Volunteer,
    "Communities In Schools of Greater New Orleans provides mentors in both Charter and Recovery District schools. Volunteer mentors meet with a child four hours every month to provide a positive role model. \\MoreWebPhone{C I S new orleans dot O R G}{504/494-0328}",
    start = "2012-08-01",
    alert = "2022-07-15",
    sourceNote = "## asked July 2015, last confirmed Dec. 2012",
    sourceContacts = Seq("Jade Parker <jparker@cisneworleans.org>")
  )

  Spot(
    "BirthingProjectGeneral",
    Volunteer,
    "The New Orleans Birthing Project provides \\emph{sister friends}, mentors for pregnant women of any age. They are seeking volunteers to be sister friends. \\MorePhoneWeb[More information about becoming or having a sister friend is]{504/482-6388}{nola at birthing project U S A dot O R G}",
    start = "2021-06-22",
    alert = "2022-07-15",
    sourceNote = "## added Dec 2011, asked Dec. 2012, asked July 2015",
    sourceContacts = "Luanne Francis <luanne_f2001@yahoo.com>"
  )

  Spot(
    "BoysHopeGirlsHope",
    Volunteer,
    "Boys Hope Girls Hope intervenes on behalf of children in need between the ages of ten and eighteen, and provides them with a stable home, positive parenting, high-quality education, and the support needed to reach their full potential. They welcome both volunteers and donations. \\MoreWeb{B H G H nola dot O R G}",
    start = "2021-06-22",
    alert = "2022-07-15",
    sourceNote = "## added August 2012, asked July 2015",
    sourceContacts = "Kelley Allenspach <kallenspach@bhgh.org>"
  )

  Spot(
    "GreenLightVolunteersTwo",
    Volunteer,
    "Volunteers with Green Light New Orleans install free energy-efficient light bulbs for any New Orleans resident. \\MorePhoneWeb[More information about the program, and about becoming a volunteer, is]{504/324-2429}{green light new orleans dot org}",
    start = "2021-06-22",
    alert = "2022-07-15",
    sourceNote = "## (from Gambit) asked July 2015, asked Dec. 2012",
    sourceContacts = Seq(
      "green@greenlightneworleans.org (Bernnel January Jr.)",
      "Andreas Hoffmann <andreashoffmann@greenlightneworleans.org"
    )
  )

  Spot(
    "GiftOfLifeMarrowTwo",
    Volunteer,
    "Gift of Life is a bone marrow and stem cell donor registry, helping children and adults with blood cancer find the matches they need, when they need them. \\MorePhoneWeb{800/9-MARROW}{gift of life dot O R G}",
    start = "2021-06-22",
    alert = "2022-07-15",
    sourceNote = "## last updated July 2015, added December 2014",
    sourceContacts = "Amy Glanzman <aglanzman@giftoflife.org>"
  )

  Spot(
    "CASAJeffFour",
    Volunteer,
    "Court-appointed special advocates help abused and neglected children to voice their needs in the legal process.  More information, including volunteering to become a court-appointed special advocate for Jefferson Parish is available by phone at 504/533-8757, or online at \\online{C A S A jefferson dot O R G}.",
    start = "2021-06-22",
    alert = "2022-07-15",
    sourceNote = "## updated January 2015 --- six-month rechecks",
    sourceContacts = Seq("Ellie Schneider <eschneider@casajefferson.org>")
  )

  Spot(
    "JeffersonAgingMealsOnWheels",
    Volunteer,
    "The Jefferson Council on Aging seeks volunteers to deliver meals to homebound adults. Gas mileage will be reimbursed. \\MorePhone[More information and volunteer scheduling are]{504/888-5880}",
    start = "2021-06-22",
    alert = "2022-07-15",
    sourceNote = "## asked July 2015, last confirmed Dec. 2012",
    sourceContacts = Seq("Liz Yager <lyager@jcoa.net>")
  )

  Spot(
    "TheNewOrleansMissionShelterTwo",
    Volunteer,
    "The New Orleans Mission, a Christian homeless shelter in Central City, is dedicated to serving the spiritual and physical needs of the poor, needy and homeless population of New Orleans. The mission provides meals, showers, clothing, shelter, literacy classes and job-skills training to men and women with children. \\MorePhoneEmail[Information about volunteering or donating is]{504/415-9579}{brittany at new orleans mission dot org}",
    start = "2021-06-22",
    alert = "2022-07-15",
    sourceNote = "## asked July 2015, last confirmed Dec. 2012",
    sourceContacts = "Brittany Ray <Brittany@neworleansmission.org>"
  )

  Spot(
    "HabForHumanFive",
    Volunteer,
    "Habitat for Humanity is in need of volunteers to achieve the goal of making decent, affordable housing a reality in the New Orleans area. No construction experience is necessary.  You can support them through volunteer labor or a donation. More information is available by phone to Cynthia White at 504/861-4121.",
    start = "2021-06-22",
    alert = "2022-07-15",
    sourceNote = "Asked July 2015, confirmed Dec. 2012",
    sourceContacts = "communications@habitat-nola.org (Billy Wells)"
  )

  Spot(
    "Gyac",
    Volunteer,
    "The Gulfsouth Youth Action Corps (``core'') seeks college student volunteers from all over the country to assist in providing recreational and educational opportunities for New Orleans-area inner-city youth and their families. \\MoreWeb{the G Y A C dot org}",
    start = "2021-06-22",
    alert = "2022-07-15",
    sourceNote = "## from Gambit, asked Dec. 2012, web site live",
    sourceURL = "http://thegyac.org/"
  )

  Spot(
    "NOLAoutreachArmsTwo",
    Volunteer,
    "New Orleans Outreach seeks volunteers for their ARMS-Outreach after-school program. Volunteers are needed in the arts, academics, technology, recreation and life skills. \\MoreWebPhone{N O outreach dot org}{504/654-1060}",
    start = "2021-06-22",
    alert = "2022-07-15",
    sourceNote = "## asked for re-confirm Dec. 2012, web site live",
    sourceContacts = "bette@nooutreach.org"
  )

  Spot(
    "YMCAyesC",
    Volunteer,
    "YMCA Educational Services, the adult literacy program of the YMCA of Greater New Orleans, offers adult education classes in reading, writing and math.  Classes are offered in Downtown New Orleans, Central City, and a new location in New Orleans East. Classes are open to the public, but since space is limited, registration is required. \\MorePhoneWeb[More information and volunteer signup are]{504/596-3842}{y m c a new orleans dot org}",
    start = "2021-06-22",
    alert = "2022-07-15",
    sourceNote = "## got update from them Jan 2014",
    sourceContacts = "Juliana Besenbruch <julieb@ymcaneworleans.org>"
  )

  Spot(
    "FreedomNotFreeOhNine",
    Volunteer,
    "The ``Freedom Is Not Free'' program helps wounded and injured veterans of all branches of the military, and their families, defray the costs associated with care-giving and travel during their most trying times.  \\MoreWeb[More information about volunteering or donating is]{freedom is not free dot com}",
    start = "2021-06-22",
    alert = "2022-07-15",
    sourceNote = "## asked Dec. 2012, no answer but web site live June 2013",
    sourceContacts = "info@freedomisnotfree.com"
  )

  Spot(
    "StairGeneral",
    Volunteer,
    "Start the Adventure in Reading, or STAIR, seeks volunteers to help second-grade public school students in the New Orleans area learn how to read.  \\MoreWebPhone[Information about tutor training sessions is]{www dot stair nola dot O R G}{504/899-0820}",
    start = "2021-06-22",
    alert = "2022-07-15",
    sourceNote = "## last confirmed Dec. 2012",
    sourceContacts = "Sara Woodard <swoodard@scapc.org>"
  )

  Spot(
    "RaintreeCFS",
    Volunteer,
    "Foster parenting is an opportunity to make a difference in a child's life.  Raintree Children and Family Services offers training and a supportive network of staff to ease the transition for new foster parents.  \\MorePhone[More information, including making a donation, is]{504/899-9045}",
    start = "2021-06-22",
    alert = "2022-07-15",
    sourceNote = "## asked Dec. 2012, no answer but web site live June 2013",
    sourceContacts = "Marlene Carter <mcarter@raintreeservices.org>"
  )

  Spot(
    "EqualityLouisiana",
    Volunteer,
    "Equality Louisiana supports policy initiatives that will make Louisiana a better place for all families. They are currently seeking volunteers all across the state. \\MoreWeb{equality L A dot O R G}",
    start = "2021-06-22",
    alert = "2022-07-15",
    sourceNote = "## added Dec. 2013",
    sourceContacts = "Micah Caswell <mcaswell@equalityla.org>"
  )

  Spot(
    "HagarHouseGeneral",
    Volunteer,
    "Hagar's House provides temporary housing to women and their children in New Orleans as they seek more permanent homes. Hagar's House offers an open and empowering residential community, support for planning and budgeting, and a safe space for transition into sustainable housing. They welcome donations and volunteers from the New Orleans community. \\MoreWebPhone{H A G A R S house nola dot O R G}{504/210-5064}",
    start = "2021-06-22",
    alert = "2022-07-15",
    sourceNote = "## asked Dec. 2012, no answer but web site live June 2013",
    sourceContacts = "hagarshouse@gmail.com"
  )

  Spot(
    "CASANolaFive",
    Volunteer,
    "CASA New Orleans is seeking volunteers, especially African-American men, to serve as advocates for children in foster care.  \\MoreEmailPhone{info at C A S A new orleans dot O R G}{504/522-1962}",
    start = "2021-06-22",
    alert = "2022-07-15",
    sourceNote = "## updated July 2015",
    sourceContacts = Seq(
      "Betsy Lopez <BLopez@casaneworleans.org>",
      "Mike Madej <mmadej@casaneworleans.org>"
    )
  )

  Spot(
    "HeartsOfChangeMentors",
    Volunteer,
    "Hearts of Change Foundation provides support services to pregnant teens and teen parents.  They are seeking successful adults who were themselves teen parents to serve as mentors for pregnant teens.  \\MoreEmailPhone{hearts of change 2012 at gmail dot com}{504/621-8894}",
    start = "2021-06-22",
    alert = "2022-07-15",
    sourceNote = "## added August 2013",
    sourceContacts = "gkannie95@yahoo.com (Kimberly Dilosa)"
  )

  Spot(
    "AidsPlanningCouncilVolunteersTwo",
    Volunteer,
    "The New Orleans Regional AIDS Planning Council plans local HIV services to help ensure that everyone infected or affected by HIV can access treatment, and live full, productive lives. \\MoreWebPhone[More information about the council and about how you can volunteer is]{N O R A P C dot O R G}{504/821-7334}",
    start = "2021-06-22",
    alert = "2022-07-15",
    sourceNote = "## asked July 2015",
    sourceContacts = "brandi@norapc.org (Brandi Bowen)"
  )

  Spot(
    "TouroVolunteers",
    Volunteer,
    "Touro Infirmary seeks adult volunteers to assist with the Family Surgery Lounge, Patient Information Desk, book and goody cart, hospital tours and health screenings. \\MorePhone{504/897-8107}",
    start = "2021-06-22",
    alert = "2022-07-15",
    sourceNote = "## (from Gambit)"
  )

  Spot(
    "RootsOfMusicGeneralTwo",
    Volunteer,
    "The Roots of Music teaches, protects and empowers at-risk youth through music education, academic support and mentorship. At the same time, they help to preserve and promote the great musical and cultural heritage of New Orleans.  \\MoreWeb[More information about events, donating or volunteering is]{the roots of music dot com}",
    start = "2021-06-22",
    alert = "2022-07-15",
    sourceContacts = "elexa@therootsofmusic.com (Elexa, no last name given)",
    sourceNote = "## asked July 2015"
  )

  Spot(
    "StompTroopers",
    Volunteer,
    "NOLArts (``NOL-arts'') Learning Center provides cultural access to young people with special needs.  Their flagship project, the STOMP Troopers, is an eight-week workshop to prepare young people with autism and other special needs to perform in the Chewbacchus Mardi Gras Parade.  \\MoreWeb[More information about donating or volunteering is]{stomp troopers dot O R G} ",
    start = "2021-06-22",
    alert = "2022-07-15",
    sourceContacts = "Kate Lacour <nolartslearningcenter@gmail.com>"
  )

  Spot(
    "MusiciansClinicGeneralSupport",
    Volunteer,
    "The New Orleans Musicians' Clinic has provided comprehensive health care and social services to thousands of local musicians, bearers of New Orleans traditions, service industry workers and many others since 1998.  You can support their mission through volunteering, or by donation. \\MoreWeb{new orleans musicians clinic dot O R G}",
    start = "2021-06-22",
    alert = "2022-07-15",
    sourceContacts = "Kari Elgin <kari@nomaf.org>"
  )

  Spot(
    "LCMtoddlerTime",
    Edu,
    "Every Thursday, the Louisiana Children's Museum hosts ``Toddler Time,'' activities for children ages 3 and under, and their parents or caregivers. Toddler Time is from ten to 10:30\\AM.  The museum is located at 420 Julia Street.  \\MorePhoneWeb{504/523-1357}{www dot L C M dot org}",
    start = "2021-06-22",
    alert = "2022-07-15",
    sourceNote = "## (with other spots from Gambit)"
  )

  Spot(
    "ToastmastersGeneral",
    Edu,
    "Toastmasters provides practice and feedback on public speaking, presentation, and communication skills.  Several groups regularly meet in the New Orleans area.  \\MoreWeb{toast masters dot O R G}",
    start = "2021-06-22",
    alert = "2022-07-15"
  )

  Spot(
    "TeacherUnionWritingWorkshop",
    Edu,
    "The United Teachers of New Orleans hosts monthly writing workshops.  The workshops are open to all New Orleans public school teachers and secondary school students. \\MorePhone{504/304-2160}",
    start = "2021-06-22",
    alert = "2022-07-15",
    sourceContacts = "Sherri Wilder <SWilder@utno.org>",
    sourceNote = "## still on in April 2010"
  )

  Spot(
    "LCMArtTrekGeneral",
    Edu,
    "Art Trek is the Louisiana Children's Museum's working art studio for drawing, painting, sculpture, screen printing, collage and more.  Programs include walk-in classes, in-depth workshops, and weeklong camps.  The curriculum focuses on the history, aesthetics, production and appreciation of art,  thinking creatively, learning about artists and art forms, and creating personal works of art.  \\MorePhone{504/586-0725, extension 212}",
    start = "2021-06-22",
    alert = "2022-07-15",
    sourceURL = "http://lcm.org/playlearn/art_programs.html",
    sourceNote = "## (with other spots from Gambit)"
  )

  Spot(
    "LCMfamilyGames",
    Edu,
    "Every Thursday, the Louisiana Children's Museum hosts ``Family Game Night,'' from four to six \\PM.  The museum is located at 420 Julia Street. \\MorePhoneWeb{504/523-1357}{www dot L C M dot org}",
    start = "2021-06-22",
    end = Some("2021-06-22"),
    alert = "2022-07-15",
    sourceNote = "From Gambit"
  )

  Spot(
    "TeacherFreeStoreOctTwenty",
    Edu,
    "The Teacher Free-Store provides quality school materials to local public school teachers.  They offer general classroom supplies, copying and printing equipment, and a lamination center.  \\MoreWeb{stem library lab dot O R G}",
    start = "2021-06-22",
    alert = "2022-07-15",
    sourceContacts = "Sean Moore <sean@stemlibrarylab.org>"
  )

  Spot(
    "FeedJusticeUUgeneral",
    Services,
    "Every month, the Feed Justice Team of the First Unitarian-Universalist Church of New Orleans hosts fresh, healthy meals to build a stronger community and neighborhood.  The meals are open to all, reflect diverse culinary traditions, and are part of the team's food justice work.  \\MoreEmail[More information about the meal, and about donating to support the meals, is]{feed justice at first U U N O dot O R G}",
    start = "2021-06-22",
    alert = "2022-07-15",
    sourceContacts = "Linda Reine <Linda@reine.biz>",
    sourceNote = "## added August 2018"
  )

  Spot(
    "VeteransHousingOutreach",
    Services,
    "Veterans Housing Outreach provides shared housing to veterans, homeless, seniors and the disabled in Orleans, Jefferson and Saint Charles Parishes.  \\MoreWebPhone[More information about their services, or about how you can support their work, is]{veterans housing outreach dot webs dot com}{504/340-3429}",
    start = "2021-06-22",
    alert = "2022-07-15",
    sourceContacts = "Lisa Carey <lisacareyrealty@yahoo.com>",
    sourceNote = "## added Feb 2014"
  )

  Spot(
    "RestaurantOpportunitiesCenter",
    Services,
    "The Restaurant Opportunities Center supports and trains New Orleans restaurant workers. They provide training in fine dining service, bartending, cooking, and English, as well as workers' rights.  \\MorePhoneWeb{504/267-4694}{R O C united dot O R G}",
    start = "2021-06-22",
    alert = "2022-07-15",
    sourceNote = "## live June 2013"
  )

  Spot(
    "HomelessVetsHotline",
    Services,
    "The US Vetarans' Administration now has a toll-free hotline to help vets who are facing homelessness. Callers will receive help with issues related to health care,  housing, employment, and education. Calls to 877/4AID-VET are confidential, and veterans are urged to use it to seek help. That number again is 877/4-A-I-D-V-E-T.",
    start = "2021-06-22",
    alert = "2022-07-15",
    sourceContacts = Seq("Stacy Vasquez 202-461-1664 (stacy.vasquez@va.gov)", "Michael Taylor 202-461-1677 (michael.taylor3@va.gov)"),
    sourceNote = "## asked June 2013"
  )

  Spot(
    "NobaSeniorDanceFitness",
    Services,
    "The New Orleans Ballet Association offers weekly dance fitness classes for older adults ages 55 and up. Programming includes stretching, dance and movement, and healthy eating and lifestyle education. \\MoreWeb{N O B A dance dot com slash senior dance fitness dot C F M}",
    start = "2021-06-22",
    end = Some("2021-07-12"),
    alert = "2022-07-15",
    sourceContacts = "Sarah Chambless Federer <sarahc@gambelpr.com>",
    sourceNote = "## add Nov 2014"
  )

  Spot(
    "CornerstoneBuildersBusProject",
    Services,
    "The Cornerstone Builders Bus Project provides a monthly bus service to bring New Orleans residents to see loved ones in Louisiana prisons.  \\MoreWeb[More information, including how you can support this service, is]{nola to angola dot O R G}",
    start = "2021-06-22",
    alert = "2022-07-15",
    sourceContacts = "Irene Rible <irene.rible@gmail.com>",
    sourceNote = "## added September 2014"
  )

  Spot(
    "NolaRecycle",
    Services,
    "The New Orleans Department of Sanitation offers a monthly recycling drop-off at 2829 Elysian Fields Avenue on the second Saturday of every month. They accept paper, plastics, metal, Mardi Gras beads, old televisions and computers, batteries, light bulbs and tires. \\MoreWeb{recycle dot nola dot gov}",
    start = "2021-06-22",
    alert = "2022-07-15",
    sourceURL = "http://recycle.nola.gov/",
    sourceNote = "## added March 2014"
  )

  Spot(
    "FHFboth",
    Services,
    "Families Helping Families provides information, training and support for families of individuals with disabilities.  Two chapters serve our area: Families Helping Families of Southest Louisiana serves Orleans, Plaquemines and Saint Bernard Parishes.  Their phone number is 504/943-0343, and their website is \\online{F H F S E L A dot O R G}.  There is also a Jefferson Parish chapter; their number is 504/888-9111, and their website is \\online{F H F jefferson dot O R G}.",
    start = "2021-06-22",
    alert = "2022-07-15",
    sourceContacts = "Madison Lagrone <mlagrone@fhfjefferson.org>",
    sourceNote = "## updated November 2014"
  )

  Spot(
    "FamilyServiceGNO",
    Services,
    "Family Service of Greater New Orleans works to strengthen emotional health and foster self-sufficiency of low-income families and individuals in New Orleans. They provide individual, family and group mental health counseling and services for anxiety, addiction, child abuse, depression, family violence, parent-child difficulties, homelessness and trauma. \\MoreWeb{F S G N O dot O R G}",
    start = "2021-06-22",
    alert = "2022-07-15",
    sourceContacts = "pr@fsgno.org",
    sourceNote = "## added March 2013"
  )

  Spot(
    "AssuranceTomorrowsLeaders",
    Services,
    "Assurance for Tomorrow's Leaders provides services to youth aged 5--18 in Orleans, Jefferson and Saint Tammany parishes, including mental health, mentoring and enrichment services. \\MorePhone{504/301-3692}",
    start = "2021-06-22",
    alert = "2022-07-15",
    sourceContacts = "Kim Byas-Dilosa <gkannie95@yahoo.com>",
    sourceNote = "## added October 2015"
  )

  Spot(
    "FairActionHousingCenter",
    Services,
    "The Fair Housing Action Center helps victims of mortgage scams, discrimination in home loans, and other forms of foul play in lending. \\MorePhoneWeb{504/596-2100}{G N O fair housing dot O R G}",
    start = "2021-06-22",
    alert = "2022-07-15",
    sourceContacts = "Stephanie Sheeley <ssheeley@gnofairhousing.org>",
    sourceNote = "## added Dec. 2012"
  )

  Spot(
    "MortgageModification",
    Services,
    "You may have been a victim of a mortgage modification or foreclosure scam if you were asked to pay a fee for your modification, sign over the title to your property, redirect mortgage payments, stop making payments, sign documents with blank spaces, or stop talking to your lender.  More information about how you can protect your home from mortgage scammers is available from the Fair Housing Action Center at 877/445-2100. ",
    start = "2021-06-22",
    alert = "2022-07-15",
    sourceContacts = "Amber Tucker <atucker@gnofairhousing.org>",
    sourceNote = "## added Jan. 2016"
  )

  Spot(
    "MakeMusicNolaB",
    Services,
    "Make Music NOLA provides music education to under-served students in New Orleans.  Started in 2012, Make Music NOLA educates hundreds of students in their string and general music programs in nine locations across the city. \\MoreWeb{make music nola dot O R G}",
    start = "2021-06-22",
    alert = "2022-07-15",
    sourceContacts = Seq(
      "Sam Rueckert <sam@makemusicnola.org>",
      "Charley Cooper/Laura Patterson <info@makemusicnola.org>",
      "laura@makemusicnola.org"),
    sourceNote = "## added Jan. 2016"
  )

  Spot(
    "Lighthouse",
    Services,
    "The Lighthouse for the Blind serves the blind and visually impaired by providing job training, competitive employment, and services.  Their visual aids store is open weekdays from eight \\AM\\ to four-thirty \\PM, and is located at 123 State Street.  \\MorePhoneWeb{504/899-4501}{lighthouse louisiana dot org}",
    start = "2021-06-22",
    alert = "2022-07-15",
    sourceNote = "## asked June 2013"
  )

  Spot(
    "FairHousingFifteen",
    Services,
    "The federal Fair Housing Act prohibits harassment based on race, color, national origin, religion, sex, disability, or family size. Harassment includes unwelcome sexual advances or verbal, physical and written threats. If you've experienced harassment from a landlord or property employee, you can get help at Fair Housing Action Center.  \\MorePhoneEmail{877/445-2100}{info at G N O fair housing dot O R G}",
    start = "2021-06-22",
    alert = "2022-07-15",
    sourceContacts = "info@gnofairhousing.org",
    sourceNote = "## received October 2015"
  )

  Spot(
    "WeHeartVeterans",
    Services,
    "We Heart Veterans provides home care for veterans over the age of 65.  Information about qualifying for their services is available by phone at 844/816-2650.",
    start = "2021-06-22",
    alert = "2022-07-15",
    sourceContacts = "Stacy Harter <sharter@wehrtvets.org>",
    sourceNote = "## added January 2015"
  )

  Spot(
    "KidCameraTwo",
    Services,
    "The New Orleans Kid Camera Project provides local children with the equipment to explore their environment and express themselves.  \\MoreWeb[More information, including a photograph display and upcoming events, is]{kid camera project dot org}",
    start = "2021-06-22",
    alert = "2022-07-15",
    copresent = "the New Orleans Kid Camera Project",
    sourceContacts = "info@kidcameraproject.org",
    sourceURL = "http://www.kidcameraproject.org/Contacts.html",
    sourceNote = "## (OK October 2008)"
  )

  Spot(
    "MetroCrisisResponse",
    Services,
    "If you or someone you know is in crisis, Metropolitan Human Services Crisis Response can help.  A crisis could mean: a danger of hurting yourself or others; an alcohol or drug abuse crisis; or feeling overwhelmed or unstable. The Metropolitan Human Services District offers crisis services 24/7 for anyone calling from Orleans, Plaquemines and Saint Bernard Parishes. Their number is 504/568-3130.",
    start = "2021-06-22",
    alert = "2022-07-15",
    sourceContacts = "Brenda.Edgerton-Webster@la.gov",
    sourceNote = "## added Feb. 12"
  )

  Spot(
    "HabForHumanOrientation",
    Services,
    "The New Orleans Area Habitat for Humanity holds open house events for prospective homeowners on the first Saturday of each month at 1911 Montegut. \\MoreWeb{habitat open house dot com}",
    start = "2021-06-22",
    alert = "2022-07-15",
    sourceContacts = "communications@habitat-nola.org",
    sourceNote = "## live June 2013"
  )

  Spot(
    "StarGeneral",
    Services,
    "STAR, the Sexual Trauma Awareness and Response team, supports survivors of sexual trauma.  They operate a 24-hour hotline for anyone needing support; their number is 855/435-STAR.  STAR is also seeking volunteers to answer their hotline and accompany survivors to the hospital. \\MoreWeb[More information about both their services and how you can help is]{star dot N G O}",
    start = "2021-06-22",
    alert = "2022-07-15",
    sourceContacts = Seq(
      "Margaret Reynolds <margaret.reynolds@star.ngo>",
      "Michaela Lovejoy <michaela.lovejoy@star.ngo>"
    ),
    sourceNote = "## added February 2016"
  )

  Spot(
    "FaithHealthAlliance",
    Services,
    "New Orleans Faith Health Alliance is a non-profit health center located in Mid-City providing primary care services to the uninsured. Services are provided on a sliding fee scale. \\MorePhone{504/486-8585}. ",
    start = "2021-06-22",
    alert = "2022-07-15",
    sourceContacts = "information@NOFHA.org",
    sourceNote = "## asked June 2013"
  )

  Spot(
    "EnvironmentProtectionAgencies",
    Services,
    "The Louisiana Department of Environmental Quality offers a reporting line for questions and concerns: 888/763-5424. The federal EPA has a hotline for hazardous waste pickup: 800/401-1327.",
    start = "2021-06-22",
    alert = "2022-07-15",
    sourceNote = "## (no email address)"
  )

  Spot(
    "ScriptsAssist",
    Services,
    "If you need help paying for prescription medicines, or have recently lost your insurance coverage, you may qualify for prescription drug support programs.  More information, is available by calling the Louisiana Partnership for Prescription Assistance, 888/477-2669.",
    start = "2021-06-22",
    alert = "2022-07-15",
    sourceNote = "## (old spot, phone number only) asked June 2013"
  )

  Spot(
    "LATANgeneral",
    Services,
    "The Louisiana Assistive Technology Access Network provides information about assistive devices, services and funding to support  normal activities like bathing, walking, reading or watching TV. \\MorePhoneWeb{800/270-6185}{L A T A N dot O R G}",
    start = "2021-06-22",
    alert = "2022-07-15",
    sourceContacts = "Maria Yiannopoulos <mariay@latan.org>",
    sourceNote = "## asked June 2013"
  )

  Spot(
    "OdysseyReentry",
    Services,
    "The Community Prisoner Re-entry program assists nonviolent, non-sex offenders with transition to the community after release from prison. This service is provided by Odyssey House Louisiana.  \\MorePhone{504/821-9211}",
    start = "2021-06-22",
    alert = "2022-07-15",
    copresent = "Odyssey House Louisiana",
    sourceContacts = "atucker@ohlinc.org",
    sourceNote = "## confirmed August 2009"
  )

  Spot(
    "ZeusRescuePetting",
    Services,
    "Zeus's (``ZOO-sez'') Rescues Uptown offers daily petting and cuddling of companion animals to reduce stress.  They are located at 2520 Napoleon Avenue, and are open most days. \\MoreWeb[More information and their open hours are]{Z E U S rescues dot O R G}",
    start = "2021-06-22",
    alert = "2022-07-15",
    sourceContacts = "Kellie Grengs <kelliegrengs@yahoo.com>"
  )

  Spot(
    "ZeusRescueGeneral",
    Services,
    "Zeus's (``ZOO-sez'') Rescues Uptown works to eradicate pet homelessness and euthanasia in the New Orleans metro area.  \\MoreWeb[More information and a link for donations are]{Z E U S rescues dot O R G}",
    start = "2021-06-22",
    alert = "2022-07-15"
  )

  Spot(
    "FreeLaMuseums",
    Services,
    "Louisiana residents can enjoy several of New Orleans' museum admission-free every week.  Visitors need only their Louisiana ID every Wednesday at the Botanical Garden and Museum of Art, every Thursday at the Ogden Museum of Southern Art, and every Sunday at the Contemporary Arts Center. \\MoreWeb{the H E L I S foundation dot O R G}",
    start = "2021-06-22",
    alert = "2022-07-15",
    sourceContacts = "Ally Hodapp <ahodapp@bondmoroch.com>"
  )

  Spot(
    "FamilyCoachingProgram",
    Services,
    "The Family Coaching and Support Program of the Louisiana Department of Health helps new and expecting parents learn the many things to know about being a parent.  Under the  program, a nurse or parent educator will come to you and help with things like having a healthy pregnancy, caring for your newborn, breastfeeding, and helping you be the best parent you can be.  The program is tailored to each family's needs, and is a service of the state of Louisiana.  \\MorePhoneWeb[More information about the program and whether you are eligible is]{504/568-5926}{partners for healthy babies dot O R G}",
    start = "2021-06-22",
    alert = "2022-07-15",
    sourceContacts = "Andrea Thames <Andrea.Thames@la.gov>"
  )

  Spot(
    "BlindRTA",
    Services,
    "The Regional Transit Authority (RTA) has launched a new Assistance Card Program for visiually impaired bus and streetcar passengers.  The program improves communication between operators and passengers by using color-coded cards to give information about the accommodations needed for the passenger.  The cards can be obtained at Lighthouse Louisiana's New Orleans location, 123 State Street, or at RTA Headquarters, 2817 Canal Street. \\MorePhone[The assistance cards, and more information about the program, are]{504/899-4501 extension 245}.  Brailed cards are available upon request.",
    start = "2021-06-22",
    alert = "2022-07-15",
    sourceContacts = "Tiffany Pounds <tpounds@lighthouselouisiana.org>"
  )

  Spot(
    "NolaAbortionFund",
    Services,
    "The New Orleans Abortion Fund is a nonprofit organization working across Louisiana to ensure that all people have access to quality medical care, regardless of their economic situation.  The Fund partners with local medical providers in Baton Rouge and New Orleans to provide financial assistance to patients seeking abortions who are unable to fully afford the cost. \\MoreWeb{new orleans abortion fund dot O R G} Their helpline for people who need assistance is 504/363-1112. Callers should leave a voicemail with name and phone number; someone will return your call within twenty-four hours.",
    start = "2021-06-22",
    alert = "2022-07-15",
    sourceNote = "Steffani Bangel <steffani@neworleansabortionfund.org>"
  )

  Spot(
    "GoGreenWaterOff",
    Eco,
    "You can save up to five gallons of water every day just by turning off the tap while brushing your teeth.  It adds up --- that's thirty five gallons a week saved during a simple everyday chore.  More tips on living green are available online at \textsl{go green NOLA dot org slash go green tips}.",
    start = "2021-06-22",
    alert = "2022-07-15",
    sourceContacts = "gogreennola@gmail.com"
  )

  Spot(
    "BuriedUtilities",
    Eco,
    "When planning to dig holes and trenches, you can call 811 to have underground utility lines and pipelines marked.  Calling 811 before digging helps prevent disruption in vital services --- and it's the law in Louisiana.  \\MoreWeb{call 8 1 1 dot com}",
    start = "2021-06-22",
    alert = "2022-07-15"
  )

  Spot(
    "CompactFlorescentsGeneral",
    Eco,
    "One of the easiest ways to lower your energy bill and fight global warming is to change the lighting in your home to compact fluorescents.  Compact fluorescents use seventy-five percent less energy than incandescent bulbs and last up to ten times longer.  If you change five of the most-used lights in your home you can save up to sixty dollars on your energy bill each year.  Each compact fluorescent prevents 450 pounds of greenhouse gas emissions over its lifetime. \\MoreWeb[More energy-saving information is]{green dot tulane dot edu}",
    start = "2021-06-22",
    alert = "2022-07-15",
    copresent = "The Tulane University Office of Environmental Affairs"
  )

  Spot(
    "GoGreenShopLocal",
    Eco,
    "By choosing local meat, seafood and produce, you support our economy, your body and the environment.  Community farmers' and seafood markets offer fresh food from our area, and our local summer produce can be canned, frozen or dried to last year-round. \\MoreWeb[More tips on eating locally are]{go green NOLA dot org slash farmers markets}",
    start = "2021-06-22",
    alert = "2022-07-15",
    sourceContacts = "gogreennola@gmail.com"
  )

  Spot(
    "NewMarreroTrashFacility",
    Eco,
    "The new Marrero Trash Drop-Off Facility is located at 6440 Lapalco Boulevard.  They offer recycling of scrap metal, appliance, tires, batteries, oil, gasoline and antifreeze.  They also have disposal containers for waste tires, waste oil, gasoline, antifreeze, and automobile batteries.  \\MorePhone{504/731-4612}",
    start = "2021-06-22",
    alert = "2022-07-15"
  )

  Spot(
    "JeffParishWaste",
    Eco,
    "Oil, paint and antifreeze should never be poured down drains or put in household trash.  Jefferson Parish residents can drop off oil, paint and antifreeze for proper disposal.  Paint may be accepted at the Green Project; for more information, call 504/945-0240.  Oil and antifreeze can be brought to the David Drive or Lapalco Boulevard trash drop-off sites.  More information is available from the Jefferson Parish Department of Environmental Affairs at 504/731-4612.",
    start = "2021-06-22",
    alert = "2022-07-15",
    sourceContacts = "RCollins@jeffparish.net"
  )

  Spot(
    "GulfRestNetFive",
    Eco,
    "We can restore Louisiana's coast, but it will take hard work and tough choices. Coastal lines of defense are natural and man-made landscape features necessary to protect coastal Louisiana. \\MoreWeb{lines of defense dot org}",
    start = "2021-06-22",
    alert = "2022-07-15",
    sourceContacts = "Robert Smith <robert@healthygulf.org>",
    sourceNote = "## (updated Feb 2012) alternate with GulfRestNetFour"
  )

  Spot(
    "GreenProjectElectronicsB",
    Eco,
    "Computers, monitors, mobile phones and other electronic devices release toxic substances like lead, cadmium, and mercury into our landfills and water when discarded improperly.  You can properly dispose of electronics at The Green Project, 2831 Marais (``MAH-ray'') Street.  \\MoreWeb{the green project dot org}",
    start = "2021-06-22",
    alert = "2022-07-15",
    sourceContacts = "ahamilton@thegreenproject.org"
  )

  Spot(
    "GoGreenCompost",
    Eco,
    "Most organic trash can be used to feed our gardens.  More information about how to set up composting in your garden, and what kitchen waste can be composted is available from \\textsl{go green nola dot org slash gardening one zero one}.",
    start = "2021-06-22",
    alert = "2022-07-15",
    sourceContacts = "gogreennola@gmail.com"
  )

  Spot(
    "GreenProjectConstructionWasteB",
    Eco,
    "The Green Project recycles building materials, which are then available for rebuilding and renovation of homes, reducing the waste going to landfills.  More information is available from The Green Project at 2831 Marais (``MAH-ray'') Street, or online at \textsl{the green project dot org}.",
    start = "2021-06-22",
    alert = "2022-07-15",
    sourceContacts = "ahamilton@thegreenproject.org",
    sourceNote = "## (added Dec. 2008)"
  )

  Spot(
    "AlcAnon",
    Health,
    "Alcoholics Anonymous offers support when drinking is no longer fun, and separates you from other people.  Their meetings offer support from others who are making their way to sobriety.  \\MorePhoneWeb[More information, and a schedule of meetings for the New Orleans area, are]{504/838-3399}{A A new orleans dot O R G}",
    start = "2021-06-22",
    alert = "2022-07-15",
    sourceContacts = "aa.nola.pi.cpc@gmail.com"
  )

  Spot(
    "DeprBipolarAllianceC",
    Health,
    "The Depression and Bipolar Support Alliance is a peer support group for people with bipolar disorder or depression, their families, and their friends.  The alliance meets the first and third Tuesdays of every month from 7:00--8:30\\PM\\ online, and periodically outside when weather permits. \\MoreWebPhone{www dot D B S A New Orleans dot org}{504/286-1916}",
    start = "2021-06-22",
    alert = "2022-07-15",
    sourceURL = "https://www.dbsaneworleans.org/"
  )

  Spot(
    "PlannedParenthood",
    Health,
    "Planned Parenthood affiliate health centers provide culturally competent, high quality, affordable health care to millions of women, men, and teens every year.  The New Orleans Planned Parenthood Center on Magazine Street is currently open with limited clinical services, including access to emergency contraception.  Emergency contraception can prevent pregnancy if taken up to one hundred twenty hours following unprotected sex.  Their numbers for medical questions, or for scheduling an appointment with the Planned Parenthood health center nearest you, are 1-800-230-PLAN and 1-800-230-7526.",
    start = "2021-06-22",
    alert = "2022-07-15",
    copresent = "Planned Parenthood",
    sourceNote = "## (no contact info)",
    boost = 0.6
  )

  Spot(
    "AccidentsHappen", Health,
    "Accidents happen.  If you have had unprotected sex in the past five days, you do not have to wait for a period that may never come. Emergency contraception can prevent pregnancy for up to one hundred twenty hours after sex.  It is available at your local Planned Parenthood office.  Their phone number is 1-800-230-PLAN.",
    start = "2021-06-22",
    alert = "2022-07-15",
    copresent = "Planned Parenthood",
    sourceNote = "## [tweets through Aug. '10]",
    boost = 0.6
  )

//    ## live meetings, pull COVID # 'NoAidsTesting',
//    # 'TargetsAdultBullying',  ## live meetings, pull COVID

  Spot(
    "BloodCenterOne",
    Health,
    "Each and every day, 300 to 350 people are needed to give blood in Southeast Louisiana.  Sixty percent of the population is eligible to donate blood, but only five percent actually do.  The Blood Center has four convenient donor centers in the New Orleans area, or our mobile team can bring the drive to you.  \\MorePhoneWeb[Center hours and addresses, and information about booking a blood drive, are]{800/86-BLOOD}{www dot the blood center dot org}",
    start = "2021-06-22",
    alert = "2022-07-15",
    copresent = "The Blood Center",
    sourceContacts = "Amy Goldfine <agoldfine@thebloodcenter.org>"
  )

  Spot(
    "StutteringFoundation",
    Health,
    "Anything that hurts your ability to communicate can limit your life. If you, your child, or someone you know has a stuttering problem, the Stuttering Foundation can help.  \\MoreWebPhone{stuttering help dot O R G}{800/992-9392}",
    start = "2021-06-22",
    alert = "2022-07-15",
    sourceContacts = "Jane Fraser <jfraser@stutteringhelp.org>"
  )

  Spot(
    "RapeCrisis",
    Health,
    "The Greater New Orleans Center for Women and Children operates programs to assist victims of domestic violence and sexual assault. Services include a 24 hour confidential crisis line, shelter for victims of domestic violence, limited legal assistance (including referrals), and counseling and advocacy services.  All services are confidential and are provided at no cost by specially trained staff members.  Help is provided both for victims, and for people who want to stop committing violent acts.  Assistance is available by calling 504/837-5400.",
    start = "2021-06-22",
    alert = "2022-07-15",
    copresent = "the Greater New Orleans Center for Women and Children",
    sourceNote = "## [tweets through June 10]",
    groupGainMultiplier = 1.2,
    boost = 0.65
  )

  Spot(
    "InternetPrescriptions",
    Health,
    "Increasing numbers of fraudulent websites are selling counterfeit medicines.  You can avoid danger when purchasing drugs online by following a few simple safety guidelines. Legitimate e-pharmacies will not allow you to purchase prescription medication without a valid prescription. Legitimate e-pharmacies should also have an easy way for you to contact a real pharmacist for a medical consultation. Risks can be further reduced by avoiding sites that do not display a physical U.S.\\ street address and a toll-free phone number, and avoiding websites that sell only ``lifestyle'' medications used to treat conditions such as obesity and impotence.  The National Association of Boards of Pharmacy has created a web site to identify online pharmacies that are legitimate and appropriately licensed at \\textsl{V I P P S dot info}.",
    start = "2021-06-22",
    alert = "2022-07-15",
    copresent = "the Partnership for Safe Medicines",
    sourceContacts = "Chad Wilkisnon <chad@keybridge.biz>"
  )

  Spot(
    "LouisianaCoalitionDomesticViolence",
    Health,
    "The Louisiana Domestic Violence Hotline is 888/411-1333. The hotline is free, confidential and available 24-hours-a-day. They can help your family, or help you to help others. Their number again is 888/411-1333.",
    start = "2021-06-22",
    alert = "2022-07-15",
    sourceContacts = "Alex Juan <alex.juan@lcadv.org>",
    groupGainMultiplier = 1.2
  )

  Spot(
    "OdysseySubstance",
    Health,
    "Odyssey House Louisiana is a nonprofit behavioral healthcare facility with an emphasis on addiction treatment, providing detoxification, residential treatment and outpatient treatment. Other services include a community medical clinic, community prisoner reentry and housing services for homeless populations.  \\MoreWebPhone{504/821-9211}{O H L I N C dot O R G}",
    start = "2021-06-22",
    alert = "2022-07-15",
    copresent = "Odyssey House Louisiana",
    sourceNote = "## confirmed August 2009 - atucker@ohlinc.org",
    sourceContacts = "atucker@ohlinc.org"
  )

  Spot(
    "OrganDonationLegacy",
    Health,
    "Today, there are nearly 100,000 people on the national organ transplant waiting list, and more than 1,700 of them live here in Louisiana.  \\MoreWeb[More information about organ and tissue donation is]{organ awareness dot org}",
    start = "2021-06-22",
    alert = "2022-07-15",
    copresent = "Legacy Donor Foundation",
    sourceContacts = "hschonekas@ontargetwithbmf.com"
  )

  Spot(
    "TulaneWomenHealthServices",
    Health,
    "The all-female staff of the Student Health Center's Women's Health Clinic provides Tulane students with STD counseling and testing, PAP exams, colposcopy, breast exams, contraceptive counseling, smoking cessation, urinary symptoms treatment, HPV vaccination, and sexual assault care.  \\MorePhone{504/865-5255}",
    start = "2021-06-22",
    alert = "2022-07-15",
    copresent = "The Tulane Women's Health Clinic"
  )

  Spot(
    "SAPHE",
    Health,
    "SAPHE (``safe''), Tulane's Sexual Aggression Peer Hotline and Education program, provides the Tulane community with resources, support and education about sexual aggression. If you or someone you know is a victim of sexual assault, relationship violence, stalking, harassment, or exploitation, you can call SAPHE's student-operated, confidential hotline at 504/654-9543. Again, the hotline's number is 504/654-9543.",
    start = "2021-06-22",
    alert = "2022-07-15",
    sourceContacts = Seq(
      "TUSAPHE@gmail.com",
      "Cory J Cole <ccole5@tulane.edu>",
      "Margaret M Mullins <mmullin5@tulane.edu>"
    ),
    boost = 0.45
  )

  Spot(
    "AmerCancerSocQuitSmoking",
    Health,
    "Smoking is the most preventable cause of death in our society.  The American Cancer Society offers information about quitting smoking, and about local resources. \\MorePhoneWeb{800/ACS-2345}{cancer dot org}",
    start = "2021-06-22",
    alert = "2022-07-15",
    sourceContacts = "Rebecca Do <rydo@loyno.edu>"
  )

  Spot(
    "TesticularCancerA",
    Health,
    "Testicular cancer is the most common cancer in men between the ages of 15 and 35.  When detected early, both sexual function and fertility can be preserved.  But if left untreated, the disease will spread, and become life-threatening.   A simple and effective self-examination is available from your doctor, or online at \\textsl{T C R C dot A C O R dot O R G}.",
    start = "2021-06-22",
    alert = "2022-07-15",
    copresent = "the Testicular Cancer Resource Center"
  )

  Spot(
    "MedicarePlanFinder",
    Health,
    "Medicare Part D, the prescription drug benefit for seniors, now offers an online service for beneficiaries to find and compare prescription drug plans.  The service, called ``Plan Finder,'' is part of the website \\textsl{medicare dov G O V}.  To use the Plan Finder, seniors will need to first make a list of their current prescriptions and dosages.  With this information, Plan Finder will return a list of appropriate insurance plans, and their costs in your area.  Plan Finder can help you navigate the hundreds of private plans competing under Medicare Part D.  \\MoreWebPhone{medicare dot G O V}{1-800-MEDICARE}",
    start = "2021-06-22",
    alert = "2022-07-15",
    copresent = "the Center for Medicine in the Public Interest"
  )

  Spot(
    "StBaldricksFoundation",
    Health,
    "The Saint Baldrick's Foundation raises money to support research into curing childhood cancer. More information about their work, making a donation, or holding a fundraiser is available on their website, \\online{S T baldricks dot O R G}.",
    start = "2021-06-22",
    alert = "2022-07-15"
  )

  Spot(
    "Prostate",
    Health,
    "Nearly two hundred thousand American will be diagnosed with prostate cancer this year, but it is over 90 percent curable when detected early.  Information on getting a simple and painless screening is available from the Prostate Cancer Education Council.  \\MorePhoneWeb{866/477-6788}{P C A W dot com}",
    start = "2021-06-22",
    alert = "2022-07-15",
    sourceURL = "https://www.pcaw.com/",
    copresent = "the Prostate Cancer Education Council"
  )

  Spot(
    "HeartDiseaseGeneral",
    Health,
    "Heart disease is the number one killer in New Orleans, Louisiana and America. It kills more people each year than all cancers combined, and does not discriminate by age, race or gender. \\MoreWeb[More information about what heart disease is, risk factors that may affect you or your loved ones, and how you can get involved to save lives is]{heart dot O R G} ",
    start = "2021-06-22",
    alert = "2022-07-15"
  )

  Spot(
    "EyesHaveIt",
    Health,
    "Children with uncorrected vision conditions can face all sorts of life barriers --- academically, psychologically and socially. Remember to have your child's eyes checked regularly.  \\MoreWeb{www dot eyes for school dot org}",
    start = "2021-06-22",
    alert = "2022-07-15",
    sourceContacts = "Susan Ilijevich <susan@whitespace-creative.com>"
  )

  Spot(
    "NarcanonScriptDrugAbuse",
    Health,
    "Prescription drug abuse is on the rise.  What's in your medicine cabinet? More information about how to keep your kids safe is available from Narconon at 877/413-3073, or online at \\textsl{drugs no dot com}.",
    start = "2021-06-22",
    alert = "2022-07-15",
    copresent = "Narcanon",
    sourceNote = "## (added Jan 09)"
  )

  Spot(
    "NationalSuicidePrev",
    Mental,
    "The National Suicide Prevention crisis counseling hotline is 800/273-8255.  This number is available both for people who are having suicidal thoughts, and who are concerned about others.  That number again is 800/273-8255.",
    start = "2021-06-22",
    alert = "2022-07-15",
    sourceNote = "## (phone number only, last called to check 8/2006)"
  )

  Spot(
    "BehavioralHealthCrisisLine",
    Mental,
    "If you or someone you know is in a behavioral health crisis, the Metropolitan Crisis Response Team can help.  A behavioral health crisis could mean: being in danger of hurting themselves or others; being in need of emergency housing; having an alcohol or drug crisis; or when a person with mental illness feels overwhelmed or unstable. The New Orleans Health Department runs a 24-hour hotline for Orleans, Plaquemines and Saint Bernard Parishes. Their number is 504/826-2675. For individuals in crisis in Jefferson Parish, the number is 504/832-5123.",
    start = "2021-06-22",
    alert = "2022-07-15",
    sourceURL = "http://new.nola.gov/health-department/behavioral-health/behavioral-health-resources/",
    groupGainMultiplier = 1.2
  )

  Spot(
    "RebuildingTogetherNewOrleans",
    Civic,
    "Rebuilding Together New Orleans is a volunteer group rehabiliting houses across New Orleans.  These repairs strengthen neighborhoods by allowing low-income homeowners to stay in their homes as they age, inhibiting blight, and keeping invested homeowners in place. \\MoreWebPhone[More information about volunteering with them is]{R T N O dot O R G}{504/264-1815}",
    start = "2021-06-22",
    end = Some("2021-07-11"),
    alert = "2022-07-15",
    sourceContacts = "Alex Thibadoux <athibadoux@rtno.org>"
  )

  Spot(
    "RebuildingTogetherNewOrleansJuly2021",
    Civic,
    "Rebuilding Together New Orleans is seeking volunteers to help with the rehabilitation of houses across New Orleans. These repairs strengthen neighborhoods by allowing low-income homeowners to stay in their homes as they age, inhibiting blight from spreading and keeping invested homeowners in place.  \\MoreWebPhone[More information about volunteering with them is]{R T N O dot O R G}{504/264-1815}",
    start = "2021-07-12",
    alert = "2022-07-15",
    sourceContacts = "Alex Thibadoux <athibadoux@rtno.org>"
  )

  Spot(
    "FriendsOfAlgiersFerryTwo",
    Civic,
    "Friends of the Ferry works to promote the Canal Street/Algiers (``al-JEERS'') Ferry as convenient and inexpensive form of public transportation.  \\MorePhoneWeb{504/363-9090}{friends of the ferry dot org}",
    start = "2021-06-22",
    alert = "2022-07-15",
    sourceNote = "## (re-confirmed October 2008)"
  )

  Spot(
    "SeatBeltBuckle", Civic,
    "New Orleanians are less likely than most to buckle up while driving. However seat belts save lives, and are required by law whether you are in the front or back seat of a vehicle.",
    start = "2021-06-22",
    alert = "2022-07-15",
    copresent = "the New Orleans Regional Traffic Safety Coalition",
    sourceContacts = "Emilie Bahr <ebahr@norpc.org>"
  )

  Spot(
    "UrbanConservancy",
    Civic,
    "The Urban Conservancy promotes the wise stewardship and equitable access for our city's and region's rich economic, environmental and cultural assets.  Current projects emphasize responsible land use to align our natural and built environments and a strong local economy. \\MoreWebEmail{urban conservancy dot O R G}{info at urban conservancy dot O R G}",
    start = "2021-06-22",
    alert = "2022-07-15",
    sourceContacts = "Dana Eness <dana@urbanconservancy.org>"
  )

  Spot(
    "IrsProvideTaxReturns",
    Civic,
    "People need copies or transcripts of prior years' tax returns for many reasons. You should keep copies of your tax returns, but if they cannot be located or have been destroyed during natural disasters or by fire, the IRS can help. You can reach the IRS for via the web, phone or by mail. Their website is \\online{I R S dot G O V}.",
    start = "2021-06-22",
    alert = "2022-07-15"
  )

  Spot(
    "LouisianaDisasterRecoveryFoundation",
    Civic,
    "The Louisiana Disaster Recovery Foundation supports long-term and equitable recovery throughout Louisiana, and provides assistance to citizens in need through a network of Louisiana charities and nonprofit agencies.  \\MoreWebPhone{louisiana help dot org}{877/435-7521}",
    start = "2021-06-22",
    alert = "2022-07-15",
    sourceNote = "## still going Aug. 09, jkemp@louisianahelp.org"
  )

  Spot(
    "TerrytownCivic",
    Civic,
    "The Terrytown Civic Association meets the first Wednesday of each month at 7\\PM\\ at the Golden Age Center, 604 Heritage Avenue.  \\MorePhoneWeb{504/914-2200}{terrytown L A dot org}",
    start = "2021-06-22",
    end = Some("2021-06-22"),
    alert = "2021-06-15",
    sourceContacts = "Stephen Leonard <president@terrytownla.org>"
  )

  Spot(
    "TulaneGalvezPheonix", Civic,
    "The Phoenix of New Orleans is the neighborhood recovery association for the Tulane/Gravier (``GRAH-vee-ur'') neighborhood, bordered by I-10, Saint Louis, Claiborne Avenue and Broad Street.  The Phoenix provides community organization, directs recovery services, and sponsors monthly neighborhood meetings.  \\MoreWebPhone[More information and volunteer opportunities are]{www dot P N O L A dot org}{504/342-4399}",
    start = "2021-06-22",
    alert = "2022-07-15",
    copresent = "The Phoenix Group",
    sourceContacts = "Paul Ikemire <director@pnola.org>"
  )

  Spot(
    "FaubourgStJohnNeighborhoodAssocTwo",
    Civic,
    "The Faubourg (``FOH-borg'') Saint John Neighborhood Association serves the areas between the Fairgrounds, City Park, Orleans Avenue and North Broad Street.  They sponsor regular meetings, cleanups, walks and other neighborhood get-togethers.  \\MoreWeb[More information and volunteer opportunities are]{F S J N A dot org}",
    start = "2021-06-22",
    alert = "2022-07-15",
    copresent = "The Faubourg (``FOH-borg'') Saint John Neighborhood Association",
    sourceContacts = "info@fsjna.org"
  )

  Spot(
    "IRSfakers",
    Civic,
    "The IRS is alerting taxpayers about email scams that direct individuals to fake websites which demand personal information like social security numbers and bank account numbers.  The IRS does not communicate by email, by text message, or over social media like Facebook. The IRS encourages people to protect their personal and financial information at all times.  Make sure you are visiting the official IRS website, \\online{I R S dot G O V}. Do not fall prey to cyberthieves by visiting fake websites ending in dot-com, dot-net, or dot-org.",
    start = "2021-06-22",
    alert = "2022-07-15",
    copresent = "the IRS"
  )

  Spot(
    "EmployExCons",
    Civic,
    "Employers: you can help reduce the recidivism rate in New Orleans by employing formerly incarcerated individuals.  Steady employment is an important factor in successfully reintegrating back into the community, and avoiding re-offending.  Federal and state funds may be available. \\MorePhone{504/568-8738}",
    start = "2021-06-22",
    alert = "2022-07-15",
    copresent = "the Department of Public Safety \\& Corrections, Division of Probation \\& Parole",
    sourceContacts = "tmccoy@corrections.state.la.us (Thomas McCoy)"
  )

  Spot(
    "LowerNinthHomeowners",
    Civic,
    "The Lower Ninth Ward Homeowners' Association meets monthly, on the second Saturday of the month, at one \\PM\\ at Holy Angels Academy, 3500 Saint Claude Avenue.",
    start = "2021-06-22",
    end = Some("2021-06-22"),
    alert = "2022-06-15",
    copresent = "the Lower Ninth Ward Homeowners' Association",
    sourceContacts = "Linda Davis <lindadavisis@yahoo.com>"
  )

  Spot(
    "ParkwayPartnersGeneral",
    Civic,
    "For thirty years, Parkway Partners has provided education and volunteers to enhance our neutral grounds, parks, schoolyards and community gardens. \\MoreWeb[More information about what they do, and how you can support them, is]{parkway partners nola dot O R G}",
    start = "2021-06-22",
    alert = "2022-07-15"
  )

  Spot(
    "NOLAneighDevColl",
    Civic,
    "New Orleans Neighborhood Development Collaborative helps low- and moderate-income families find high-quality housing. They are currently offering grants to qualified families including Section 8 and public housing residents. \\MoreWebPhone{harmony homes nola dot com}{504/302-8696}",
    start = "2021-06-22",
    alert = "2022-07-15"
  )

  Spot(
    "NolaThreeOneOne",
    Civic,
    "NOLA 311 handles non-emergency city service requests such as City Assisted Evacuation registration, code enforcement complaints, street and sidewalk maintenance, street and traffic signs, traffic signals, street lights, abandoned vehicles and sanitation issues.  You can reach them weekdays from 8\\AM\\ to 5\\PM\\ by dialing 311 from any phone based in New Orleans, or 504/658-2299, or 877/286-6431.",
    start = "2021-06-22",
    alert = "2022-07-15",
    sourceNote = "## added Sept 2012"
  )

  Spot(
    "ArabAntiDiscTwo",
    Civic,
    "In recent years, bias against Arab Americans has grown throughout the US.  The American Arab Anti-Discrimination Committee (ADC) is a grassroots civil rights organization working to combat discrimination, bias and stereotyping.  \\MoreWeb[More information about the New Orleans chapter of the ADC is]{A D C dot org}",
    start = "2021-06-22",
    alert = "2022-07-15",
    copresent = "American Arab Anti-Discrimination Committee",
    sourceContacts = "info@nolahumanrights.org"
  )

  Spot(
    "TransparencyAccountability",
    Civic,
    "The Project To Promote Transparency and Accountability in Government holds a series of meetings at which residents are invited to offer their input into the city's priority-setting and decision-making. \\MorePhone{504/267-4665 or 267-4696}",
    start = "2021-06-22",
    alert = "2022-07-15",
    sourceNote = "## (from the Times-Pic)"
  )

  Spot(
    "YouthEmpowermentProjectBikes",
    Civic,
    "The Youth Empowerment Project serves at-risk New Orleans youth by providing professional training in a bike shop setting. They are seeking donations of used bicycles and parts in any condition for their training program. \\MorePhoneWeb[More information, plus scheduling for donation pick-up or drop-off, are]{504/875-4301}{youth empowerment project dot O R G}",
    start = "2021-06-22",
    alert = "2022-07-15",
    sourceNote = "bwhite@youthempowermentproject.org"
  )

  Spot(
    "FederalDepositoryLibrary",
    Civic,
    "Are you interested in information on science, technology, health, laws, the workings of the presidential administration, and more?  Your local Federal Depository Library provides open access to U.S.\\ government information.  You can find the library nearest you at \\online{G P O dot G O V slash libraries}.",
    start = "2021-06-22",
    alert = "2022-07-15"
  )

  Spot(
    "CentralCityRenaissance", Civic,
    "The Central City Renaissance Alliance meets monthly to discuss community issues and resources available to Central City.  Information about their next meeting is on their answering machine at 504/581-5301.  More information about the Alliance is available at the same number, 504/581-5301.",
    start = "2021-06-22",
    alert = "2022-07-15",
    copresent = "the Central City Renaissance Alliance",
    sourceContacts = "Dorian Hastings <d_hastings@juno.com>"
  )

  Spot(
    "MidCityCommunityMeeting", Civic,
    "Mid-City Recovery Action Meetings take place on the first Monday of each month at 6:30\\PM\\ at Grace Episcopal Church, 3700 Canal Street.  Their meetings are open to the public, and all are invited to help rebuild Mid-City. More information is available on the Mid-City Neigbhorhood Organization website, \\textsl{M C N O dot org}.",
    start = "2021-06-22",
    end = Some("2021-06-22"),
    alert = "2021-05-15",
    copresent = "Mid-City Neigbhorhood Organization",
    sourceContacts = "Bart <b@rox.com>"
  )

  Spot(
    "PAWS",
    Animal,
    "PAWS, the Plaquemines (``PLACK-minzz'') Animal Welfare Society, cares for unwanted, abused and abandoned animals in Plaquemines (``PLACK-minzz'') Parish, and works to find new homes for these animals. \\MoreWeb{paws 4 life dot O R G}",
    start = "2021-06-22",
    alert = "2022-07-15",
    sourceContacts = "Kathy Fleming <kathylee1145@yahoo.com>"
  )

  Spot(
    "SPCAone",
    Animal,
    "The Louisiana SPCA needs volunteers. Volunteers can help by responding to telephone inquiries, assisting with arriving animals, exercising and socializing animals, providing foster care and vet clinic assistance, in addition to many other roles.  \\MoreWeb[More information and a volunteer application is]{L A hyphen S P C A dot org}",
    start = "2021-06-22",
    alert = "2022-07-15",
    copresent = "the Louisiana SPCA"
  )

  Spot(
    "HumaneSocLou",
    Animal,
    "The Humane Society of Louisiana investigates animal cruelty, rescues and fosters abused and neglected animals, and provides animal care information to the public.  They are seeking foster homes, volunteers and donations.  \\MoreWebPhone{humane L A dot org}{888/6-HUMANE}",
    start = "2021-06-22",
    alert = "2022-07-15",
    copresent = "Humane Society of Louisiana",
    sourceContacts = "Robyn B. <marshmallowlullaby@hotmail.com>"
  )

  Spot(
    "SPCAtwoRevOhSeven",
    Animal,
    "The Louisiana SPCA can help when someone loses a pet, knows of an animal needing help, or needs to report an animal control issue.  Their number is 504/368-5191.  The LSPCA can also assist pet owners trying locate pets lost after Katrina.  Their number again is 504/368-5191.",
    start = "2021-06-22",
    alert = "2022-07-15",
    copresent = "the Louisiana SPCA"
  )

  Spot(
    "MicrochippingSPCA",
    Animal,
    "Microchipping is a quick, easy way to ensure that your dog or cat will never be without their identifying information.  The Louisiana SPCA partners with local businesses to offer low-cost microchipping events around the New Orleans metro area.    \\MoreWeb[The time and place of the next microchipping event is]{www dot L A hyphen S P C A dot org}",
    start = "2021-06-22",
    alert = "2022-07-15",
    copresent = "the Louisiana SPCA"
  )

  Spot(
    "SpaymartTwo",
    Animal,
    "Spaymart Sanctuary is a New Orleans non-profit cat adoption and spay neuter organization.  They are seeking volunteers and prospective cat owners.  \\MoreWebPhone{spaymart dot org}{601/749-0268}",
    start = "2021-06-22",
    alert = "2022-07-15",
    copresent = "Spaymart",
    sourceContacts = "Lynn Chiche <spaymart@hughes.net>"
  )

  Spot(
    "AnimalRescueNewOrleansB",
    Animal,
    "Animal Rescue New Orleans is a nonprofit no-kill animal shelter located at 271 Plauche (``PLAW-shay'') Street, off Jefferson Highway in Harahan, near the Huey P.\\ Long Bridge. They need volunteers to assist with office tasks, to care for cats and dogs, and to walk dogs in the evenings from 5:30-8:30.  All volunteers are welcome, and no advance signup is required.  \\MoreEmail{A R new orleans at aol dot com}",
    start = "2021-06-22",
    end = Some("2021-06-22"),
    alert = "2021-06-15",
    sourceNote = "Kathy Fleming <kathylee1145@yahoo.com>"
  )

  Spot(
    "DagsHouse",
    Animal,
    "Dag's House provides information, education and services for dog owners to improve the quality of life of their dogs. \\MoreWeb[More information about their services, or about volunteering with them, is]{D A G S house dot com}",
    start = "2021-06-22",
    alert = "2022-07-15",
    sourceContacts = "Kathy Fleming <kathylee1145@yahoo.com>"
  )

  Spot(
    "BoxerRescue",
    Animal,
    "Louisiana Boxer Rescue works to rescue, rehabilitate and find new home for abandoned, neglected and abused boxers. \\MoreWeb[More information and volunteer opportunities are]{louisiana boxer rescue dot O R G}",
    start = "2021-06-22",
    alert = "2022-07-15",
    sourceNote = "kathy fleming <kathylee1145@yahoo.com>"
  )

  val nomuseumsdotcomblurb: String = """The New Orleans Tourism Marketing Corporation maintains a directory of city and area museums online at \textsl{New Orleans Museums dot com}."""

  Spot(
    "TulaneSpecialCollectionsRegular",
    Museum,
    "Tulane University's Howard-Tilton Memorial Library offers a Special Collections Division for student and professional research. The Hogan Jazz Archive preserves oral histories, recordings, sheet music, and images, about Jazz in New Orleans.  The Louisiana Collection contains books, pamphlets, maps, sheet music, newspapers, photographs and other historical printed materials from French explorations to the present. The special collections also include the manuscripts department, university archives, rare books and the architectural archives.  \\MorePhoneWeb{504/865-5685}{special collections dot tulane dot edu}  " + nomuseumsdotcomblurb,
    start = "2021-06-22",
    alert = "2022-07-15",
    sourceURL = "http://www.neworleansmuseums.com/"
  )

  Spot(
    "PrcRegular",
    Museum,
    "The Preservation Resource Center features both permanent and rotating exhibits on New Orleans architecture and historic neighborhoods.  The ``Living With History'' exhibit features hundreds of professional and amateur photographs of New Orleans residents going about their daily lives in several of New Orleans' fascinating neighborhoods.  The center also provides walking tour brochures highlighting neighborhood restaurants, churches, theaters and more.  The Preservation Resource Center is located at 923 Tchoupitoulas Street in the Warehouse District.  \\MorePhoneWeb{504/581-7032}{www dot P R C N O dot org}  " + nomuseumsdotcomblurb,
    start = "2021-06-22",
    alert = "2022-07-15",
    sourceURL = "Kathy Fleming <kathylee1145@yahoo.com>"
  )

  Spot(
    "BeauregardKeyesRegular",
    Museum,
    "The Beauregard-Keyes (``BO-regard'') House, built in 1826 and named for two of its former tenants, is a fine example of a raised center hall house.  Visitors can explore the original furnishing of General Beauregard, and Mrs.\\ Keyes' collections of antiques.  \\MorePhone{504/523-7275}  " + nomuseumsdotcomblurb,
    start = "2021-06-22",
    alert = "2022-07-15",
    sourceURL = "http://www.neworleansmuseums.com/"
  )

  Spot(
    "DegasHouseRegular",
    Museum,
    "The Edgar Degas House was the home of the impressionist painter during his time in New Orleans, and is the only known home or studio of Degas that is open to the public. Tours of the home are conducted by appointment only, include a viewing of the award-winning documentary \\emph{Degas in New Orleans, A Creole Sojourn}, and support the Edgar Degas Foundation.  \\MorePhoneWeb{504/821-5009}{www dot degas house dot com}  " + nomuseumsdotcomblurb,
    start = "2021-06-22",
    alert = "2022-07-15",
    sourceURL = "http://www.neworleansmuseums.com/"
  )

  Spot(
    "PitotHouseRegular",
    Museum,
    "The Pitot House Museum on Bayou (``BUY-you'') Saint John is an eighteenth-century Creole colonial building, and was the home of New Orleans first mayor.  The museum has been restored to highlight its distinctive construction and roof, and is furnished with Louisiana and American antiques from the early 1800s.  Visitors do need an appointment to tour the home. \\MorePhoneWeb[More information and appointments are]{504/482-0312}{www dot P I T O T house dot org}  " + nomuseumsdotcomblurb,
    start = "2021-06-22",
    alert = "2022-07-15",
    sourceURL = "http://www.neworleansmuseums.com/"
  )

  Spot(
    "JazzParkRegular",
    Museum,
    "In a city famous for jazz halls and great music, there is a place we can go to learn how New Orleans came to be the birthplace of jazz.  The New Orleans Jazz National Historical Park preserves information and resources related to the beginnings and progressions of jazz in New Orleans. The visitor center is located at 916 North Peters Street is the French Quarter.  \\MorePhoneWeb{504/589-4806}{www dot N P S dot gov slash jazz}  " + nomuseumsdotcomblurb,
    start = "2021-06-22",
    alert = "2022-07-15",
    sourceURL = "http://www.neworleansmuseums.com/"
  )

  Spot(
    "DDayMuseumRegular",
    Museum,
    "New Orleans is home to the country's official World War II Museum.  Permanent exhibits tell the story of both fronts of the war from both the military and civilian points of view. Temporary exhibits and lectures delve more deeply into aspect of that era.  \\MorePhoneWeb{504/527-6012}{www dot national w w 2 museum dot org}  " + nomuseumsdotcomblurb,
    start = "2021-06-22",
    alert = "2022-07-15",
    sourceURL = "http://www.neworleansmuseums.com/"
  )

  Spot(
    "AmericanItalianMuseumRegular",
    Museum,
    "The American-Italian Renaissance Foundation's Museum tells the story of the cultural contributions of one of New Orleans' more often-overlooked immigrant groups --- from procedures in anesthesiology which are still in use today, to the tradition of the Saint Joseph's Day altar.  The Museum is located at 537 South Peters in the Warehouse District.  \\MorePhoneWeb{504/522-7294}{www dot A I R F dot org}  " + nomuseumsdotcomblurb,
    start = "2021-06-22",
    alert = "2022-07-15",
    sourceURL = "http://www.neworleansmuseums.com/"
  )

  Spot(
    "PharmacyMuseumRegular",
    Museum,
    "The New Orleans Pharmacy Museum is the largest and finest pharmaceutical collection in the United States.  Housed in the French QUarter, in the 1823 apothecary of America's first licensed pharmacist, the Museum contains a collection of 19th-century pharmacy and medical artifacts including an exhibition on epidemics in New Orleans. \\MorePhoneWeb{504/565-8027}{www dot pharmacy museum dot org}  " + nomuseumsdotcomblurb,
    start = "2021-06-22",
    alert = "2022-07-15",
    sourceURL = "http://www.neworleansmuseums.com/"
  )

  Spot(
    "LongueVueHouseRegular",
    Museum,
    "Longue Vue (``long view'' or ``long voo'') House in one of the few surviving examples of Country Era Place homes in the south, and has been restored in the style of the early 20th Century.  The house and its extensive gardens, as well as the home's Discovery Gardens, a hands-on learning environment for children, are part of the home's public tour.  \\MorePhoneWeb{504/488-5488}{www dot L O N G U E V U E dot com}  " + nomuseumsdotcomblurb,
    start = "2021-06-22",
    alert = "2022-07-15",
    sourceURL = "http://www.neworleansmuseums.com/"
  )

  Spot(
    "DDayMuseumLivingHistoryCorps",
    Museum,
    "The National World War II Museum's World War II re-enactors, collectively known as the Living History Corps (``core''), are local volunteers who enrich visitors' museum experience with their wealth of information and stories. Wearing the uniforms and carrying the equipment of both the Allied and Axis forces, they share their knowledge about the day-to-day lives of military men and women, and the broader lessons of World War II.  Living History Corps (``core'') events are open to the public. \\MorePhoneWeb{504/527-6012 extension 333}{national W W 2 museum dot org slash calendar}",
    start = "2021-06-22",
    alert = "2022-07-15",
    sourceURL = "http://www.nationalww2museum.org/calendar/"
  )

  Spot(
    "AmistadRegular",
    Museum,
    "The Amistad Research Center houses the country's largest collection of manuscripts about African Americans, race relations and civil rights.  This center is a research resource for historians, novelists, and individual pursuing information about their family's history.  Guided tours are availabel, but must be scheduled at least two weeks in advance.  \\MorePhoneWeb{504/865-5535}{www dot tulane dot edu slash tilde amistad}  " + nomuseumsdotcomblurb,
    start = "2021-06-22",
    alert = "2022-07-15",
    sourceURL = "http://www.neworleansmuseums.com/"
  )

  Spot(
    "TulaneNaturalHistoryRegularB",
    Museum,
    "The Tulane Museum of Natural History houses extensive collections of amphibians, invertebrates, fish, birds, mammals, reptiles and fossils.  The Museum does not keep public open hours, but individual appointments and arrangements for school tours and scholarly researchers can be made. \\MorePhoneWeb{504/394-1711}{www dot museum dot tulane dot edu}",
    start = "2021-06-22",
    alert = "2022-07-15",
    sourceURL = "http://www.neworleansmuseums.com/"
  )

  Spot(
    "HistoricNewOrleansRegular",
    Museum,
    "The Historic New Orleans Collection houses primary sources including manuscripts and documents, literary and artistic treasures, and other artifacts showcasing the numerous cultures that shaped New Orleans from the 18th century to the present.  Along with public exhibition galleries and a museum shop, the Collection offers A research guide and photocopying services for scholarly and historic research. \\MorePhoneWeb{504/523-4662}{www dot H N O C dot org}  " + nomuseumsdotcomblurb,
    start = "2021-06-22",
    alert = "2022-07-15",
    sourceURL = "http://www.neworleansmuseums.com/"
  )

  Spot(
    "OgdenAfterHours",
    Museum,
    "Every Thursday evening, The Ogden Museum of Southern Art presents \\emph{Ogden After Hours}, music in the atrium of the museum.  During these concerts, the museum also offers art activities for kids.  \\MorePhoneWeb{504/539-9600}{www dot ogden museum dot org}  " + nomuseumsdotcomblurb,
    start = "2021-06-22",
    alert = "2022-07-15",
    sourceURL = "http://www.neworleansmuseums.com/"
  )

  Spot(
    "JeanLafitteRegular",
    Museum,
    "The Jean Lafitte National Historical Park and Preserve consists of six sites in Louisiana, three of which are in the metropolitan New Orleans area: the Laura C. Hudson Visitor Center in the French Quarter, the Barataria Preserve, and the Chalmette Battlefield and National Cemetery. \\MorePhoneWeb[More information about any of these sites is] {504/589-3882}{www dot N P S dot gov slash J E L A}  " + nomuseumsdotcomblurb,
    start = "2021-06-22",
    alert = "2022-07-15",
    sourceURL = "http://www.neworleansmuseums.com/"
  )

  Spot(
    "CabildoRegular",
    Museum,
    "The Cabildo is one of the most historically significant buildings in America.  It was the seat of the Spanish Colonial government, the site of the Louisiana Purchase transfer in 1803, and a home to the Louisiana State Supreme Court.  Now, the Cabildo is a public museum showcasing the rich and colorful textures of Louisiana's history.  \\MorePhoneWeb{800/568-6968}{L S M dot C R T dot state dot L A dot U S}  " + nomuseumsdotcomblurb,
    start = "2021-06-22",
    alert = "2022-07-15",
    sourceURL = "http://www.neworleansmuseums.com/"
  )

  Spot(
    "ChildrensMuseumRegular",
    Museum,
    "The Louisiana Children's Museum offers more than 30,000 square feet of hands-on, interactive exhibits that invite and engage children and families as they explore art, music, science, math, and health, and role-playing environments. The museum is located at 420 Julia Street in the Warehouse District, is open seven days a week in the summer, and closes Mondays during the school year.  \\MorePhoneWeb{504/523-1357}{www dot L C M dot org}  " + nomuseumsdotcomblurb,
    start = "2021-06-22",
    alert = "2022-07-15",
    sourceURL = "http://www.neworleansmuseums.com/"
  )

  Spot(
    "BackstreetRegular",
    Museum,
    "The Backstreet Cultural Museum is home to an amazing assortment of memorabilia of Mardi Gras, jazz funerals and other traditions found only in New Orleans, including the city's largest collection of Mardi Gras Indian costumes. \\MorePhoneWeb[Directions to the museum and more information are]{504/522-4806}{www dot back street museum dot org}  " + nomuseumsdotcomblurb,
    start = "2021-06-22",
    alert = "2022-07-15",
    sourceURL = "http://www.neworleansmuseums.com/"
  )

  Spot(
    "AfAmMuseumRegular",
    Museum,
    "The New Orleans African American Museum is dedicated to protecting, preserving, and promoting through education the history, art, and communities of African Americans in New Orleans and the African diaspora.  Five restored building over a full city block in the Tr\\`eme (``TREHM-ay'') host permanent and changing exhibits, and a serene garden.  Tour groups, school groups and individuals are welcomed by appointment only.  \\MorePhoneWeb[Information and appointments are]{504/566-1136}{www dot N O A A M dot org}  " + nomuseumsdotcomblurb,
    start = "2021-06-22",
    alert = "2022-07-15",
    sourceURL = "http://www.neworleansmuseums.com/"
  )

  Spot(
    "SculptureGardenRegular",
    Museum,
    "The Besthoff Sculpture Garden occupies five acres of City Park, and is open to the public five days a week, Wednesdays through Sundays.  The collection includes work by some of the great master sculptors of the twentieth century, as well as younger, contemporary sculptors.  \\MoreWeb{www dot N O M A dot org}  " + nomuseumsdotcomblurb,
    start = "2021-06-22",
    alert = "2022-07-15",
    sourceURL = "http://www.neworleansmuseums.com/"
  )

  Spot(
    "OgdenRegular",
    Museum,
    "The Ogden Museum of Southern Art is home to the most comprehensive collection of southern art in the world, spanning fifteen states and four centuries.  The Ogden features special programming for visitors of all ages, plus weekly evening gallery openings and music performances.  \\MorePhoneWeb{504/539-9600}{www dot ogden museum dot org}  " + nomuseumsdotcomblurb,
    start = "2021-06-22",
    alert = "2022-07-15",
    sourceURL = "http://www.neworleansmuseums.com/"
  )

  Spot(
    "NOMAregular",
    Museum,
    "The New Orleans Museum of Art is the premier art museum of the Gulf South region.  NOMA's broad collection includes a notable collection of Faberg\\'e\\ eggs and treasures, and the Latin American Colonial collection.   \\MoreWebPhone{www dot N O M A dot org}{504/488-2631}  " + nomuseumsdotcomblurb,
    start = "2021-06-22",
    alert = "2022-07-15",
    sourceURL = "http://www.neworleansmuseums.com/"
  )

  Spot(
    "DibollRegular",
    Museum,
    "The Collins C.\\ Diboll Art Gallery is a small museum dedicated to artifacts of Belgian Congo plus rotating exhibits, including exhibits of Loyola student and faculty work.  The gallery is located in the Monroe Library on Loyola's campus. \\MoreWebPhone{www dot L O Y N O dot edu}{504/861-5456}  " + nomuseumsdotcomblurb,
    start = "2021-06-22",
    alert = "2022-07-15",
    sourceURL = "http://www.neworleansmuseums.com/"
  )

  Spot(
    "NewcombArtRegular",
    Museum,
    "The Newcomb Art Gallery showcases a permanent collection of nineteenth- and twentieth-century pieces produced at Newcomb College, in addition to hosting quarterly traveling exhibitions featuring mostly contemporary artists.  The world-renowned Newcomb Pottery section represents the largest area of holdings at the museum and features several rare exhibition-only and early experimental pieces. \\MoreWebPhone{www dot newcomb art gallery dot com}{504/865-5328}  " + nomuseumsdotcomblurb,
    start = "2021-06-22",
    alert = "2022-07-15",
    sourceURL = "http://www.neworleansmuseums.com/"
  )

  Spot(
    "CACregular",
    Museum,
    "The Contemporary Arts Center is a multidisciplinary arts center presenting an array of programs encompassing the visual arts, music, dance and drama, all celebrating the arts of our time.  \\MoreWebPhone{www dot C A C N O dot org}{504/528-3805}  " + nomuseumsdotcomblurb,
    start = "2021-06-22",
    alert = "2022-07-15",
    sourceURL = "http://www.neworleansmuseums.com/"
  )

  Spot(
    "HermannGrimaHouseRegular",
    Museum,
    "Located in the heart of the historic French Quarter, the Hermann-Grima House is thought to be the finest example of American architecture in the area.  The mansion has accurately restored to depict the gracious lifestyle of a wealthy Creole family from 1830 to 1860. Visitors can tour the original stables, the outdoor kitchen with the open hearth for cooking, and a meticulously restored courtyard with citrus trees and antique roses.  \\MoreWebPhone{www dot H G G H H dot org}{504/525-5661}  " + nomuseumsdotcomblurb,
    start = "2021-06-22",
    alert = "2022-07-15",
    sourceURL = "http://www.neworleansmuseums.com/"
  )

  Spot(
    "GallierHouseRegular",
    Museum,
    "Gallier (``GAL-yer'') House, built in 1857, was described as ``one of thebest small museums in the country'' by the \\emph{New York Times}.  The house is a well-preserved example of progressive design, and is decorated throughout the year to reflect period seasonal styles.  \\MoreWebPhone[More information about Gallier (``GAL-yer'') House is]{www dot H G G H H dot org}{504/525-5661}  " + nomuseumsdotcomblurb,
    start = "2021-06-22",
    alert = "2022-07-15",
    sourceURL = "http://www.neworleansmuseums.com/"
  )

  Spot(
    "ShalamarWritingWorkshop",
    Rare,
    "Shalamar Publishing holds writing workshops to offer feedback and advice to local writers.  Workshops are open to the public, and are held the first Sunday of each month at 6\\PM\\ at the CC's Coffee at 3647 Veterans Memorial Boulevard in Metairie.",
    start = "2021-06-22",
    end = Some("2021-06-22"),
    alert = "2021-06-15",
    sourceContacts = "Deborah Dixon <deboracracy@gmail.com>"
  )

  Spot(
    "MicrowaveBoilingWater", Rare,
    "Boiling water in a microwave oven may seem easy and convenient --- but serious burns are never easy, or convenient.  Water heated by microwaves can \\emph{superheat}, leading to an explosion of steam and hot water when the container is picked up.  You can keep water from superheating by heating it in a wide-mouthed container like a mixing cup, never a narrow-mouthed container like a jar, and adding a wooden popsicle stick or toothpick to the water, to give bubbles a place to form.  Remember: no matter much of a hurry you're in, you don't have time for superheated microwave water scalding.",
    start = "2021-06-22",
    alert = "2022-07-15"
  )

  Spot(
    "JobSeekersNOLA",
    Rare,
    "The Job Seekers Alliance is a networking group for New Orleans area individuals looking for work in the non-profit sector.  The group meets on the first and third Friday of each month from ten to eleven \\AM\\ at 1824 Oretha Castle Haley Boulevard.  \\MoreWeb{J S A nola at gmail dot com}",
    start = "2021-06-22",
    alert = "2022-07-15",
    copresent = "the New Orleans Job Seekers Alliance",
    sourceContacts = "Heather Mack <jsanola@gmail.com>"
  )

  Spot(
    "RockTheEarth",
    Rare,
    "Some of the best music in the world invokes a sense of time and place in the listener.  Today many of our planet's special places are under increased stress and your help is needed to protect the areas that we all cherish.  Rock the Earth and their musical partners are working hard in the name of our most precious natural places.  \\MoreWeb[More information about their work and how you can help are]{rock the earth dot org}",
    start = "2021-06-22",
    alert = "2022-07-15",
    sourceContacts = "glennf@rocktheearth.org"
  )

  Spot(
    "AgendaForChildren",
    Rare,
    "Agenda for Children advocates for children's programs in Louisiana for education, health, adoption and foster care, and the justice system. \\MoreWeb[More information, including how you can support their mission, is]{agenda for children dot O R G} ",
    start = "2021-06-22",
    alert = "2022-07-15",
    sourceContacts = "Erica Severson <SeversonE@peteramayer.com>"
  )

  Spot(
    "SBAbeforeDistaster", Rare,
    "Before a disaster strikes, there are things you can do to protect yourself.  The U.S.\\ Small Business Administration encourages homeowners and small businesses to develop a disaster preparedness plan.  Preparation includes: having a battery operated radio on hand to monitor news and weather reports;  identifying possible hazards;  familiarizing yourself with escape routes for use in an emergency;  keeping phone numbers handy; and saving copies of financial records offsite to help speed up the recovery process.  \\MoreWeb{www dot S B A dot gov}",
    sourceContacts = "Aaron Viles <aaron@healthygulf.org>",
    copresent = "U.S.\\ Small Business Administration",
    start = "2021-06-22",
    alert = "2022-07-15"
  )

  Spot(
    "Eracism",
    Rare,
    "Eracism is the slogan of the group ERACE, which was formed in New Orleans in 1993. Their mission is to seek ways through person-to-person communication to show that they are committed to treating fellow human beings of all colors with love and respect. \\MorePhone[More information about ERACE, its discussion meeting schedule, or bumper stickers is]{504/866-1163}",
    start = "2021-06-22",
    alert = "2022-07-15"
  )

  Spot(
    "IrsWebSite",
    Taxtime,
    "The IRS website, \\online{I R S dot G O V} is a one-stop shop for an array of tax information. You can even prepare and file your federal tax return through ``Free File,'' a service offered by IRS and its partners who offer tax preparation software and electronic filing as a public service. The site also includes answers to common tax questions, and lets you check the status of your refund. The IRS website is \\online{I R S dot G O V}.",
    start = "2021-06-22",
    alert = "2022-07-15"
  )

  Spot(
    "IrsMilitaryHelp", Taxtime,
    "Free tax return preparation assistance is available for eligible military members and their spouses. Volunteers at military VITA sites are trained to address military-specific tax issues, such as combat zone tax benefits and the Earned Income Tax Credit guidelines. More information is available in IRS Publication 3, the \\emph{Armed Forces' Tax Guide}, available on the IRS web site \\online{I R S dot G O V}, or by phone, 800/TAX-FORM, that's 800/829-3676.",
    start = "2021-06-22",
    alert = "2022-07-15"
  )

  Spot(
    "IrsElderlyAssistance",
    TaxAlways,
    "Free tax preparation is available through the Volunteer Income Tax Assistance and Tax Counseling for the Elderly programs in many communities. Volunteer return preparation programs are provided through partnerships between the IRS and community-based organizations. They offer free help in preparing simple tax returns for low-to-moderate income taxpayers. The New Orleans office is located at 1555 Poydras (``POI-druhss'') Street, and their phone number is 504/558-3344. \\MoreWebPhone[More information including other locations is available]{I R S dot G O V}{800/906-9887}",
    start = "2021-06-22",
    alert = "2022-07-15"
  )

  Spot(
    "IrsPhoneNumbers",
    TaxAlways,
    "The IRS operates a number of phone services for taxpayers. Staff will answer your federal tax questions on the tax help line for individuals, 800/829-1040.  Pre-recorded messages covering various tax topics, plus automatic checking of the status of your refund, are available at 800/829-4477.  You can order forms, instructions and publications at 800/829-3676. Finally, TTY and TDD users may call 800/829-4059 for tax questions and ordering forms and publications.",
    start = "2021-06-22",
    alert = "2022-07-15"
  )

  Spot(
    "IrsHelpInPerson",
    TaxAlways,
    "If you believe your tax issue cannot be handled online or by phone, and you want face-to-face assistance, you can find help at a local IRS Taxpayer Assistance Center. The New Orleans office is located at 1555 Poydras (``POI-druhss'') Street, and their phone number is 504/558-3344. Other locations, business hours and an overview of services are available online at \\online{I R S dot G O V}, under the ``Individuals'' tab, and ``Contact My Local Office link.''",
    start = "2021-06-22",
    alert = "2022-07-15"
  )

  Spot(
    "IrsScamAwareness",
    TaxAlways,
    "The IRS warns taxpayers to be wary of tax scams. They issue an annual ``Dirty Dozen'' tax scams list ranging from identity theft to return preparer fraud. More information, including warning signs and new scams, are available by search on the IRS website, \\online{I R S dot G O V}.",
    start = "2021-06-22",
    alert = "2022-07-15"
  )

  Spot(
    "FemaApp",
    StormPrep,
    "FEMA, the Federal Emergency Management Agency, now has an app for hurricane season readiness.  You can get alerts from the National Weather Service, plus safety reminders, emergency checklists, and information about shelters.  More information about the app is available at \\online{fema dot G O V slash mobile hyphen app}.",
    copresent = "FEMA",
    start = "2021-07-12",
    alert = "2022-05-15"
  )

  Spot(
    "FemaStormDocPrep",
    StormPrep,
    "If you need to evacuate for a hurricane, having important documents ready to go can help get your recovery process started quickly and efficiently in the worst cases.  Keep important papers in a fireproof, waterproof box or safe in your home next to the emergency kit. Store copies in an alternate location such as a safe deposit box, work place or trusted friend or family member's home.  You can make electronic copies and keep them on a flash drive or CD in your emergency kit, email copies to yourself, or upload them to a cloud storage service.  Other important documents to include are: birth certificates, passports, Social Security cards; insurance policies; deed, mortgage, lease and loan papers; lists of medications, allergies and medical equipment; photos of valuable belongings you may want to include in an insurance claim; and contact information for doctors, relatives, creditors and utilities.  More information on preparing for hurricanes is available at \\online{ready dot G O V slash hurricanes}~.",
    copresent = "FEMA",
    start = "2021-07-12",
    alert = "2022-05-15"
  )

  Spot(
    "FemaContractorWarning",
    StormPrep,
    "After a hurricane or other natural disaster, survivors should ber wary of post-disaster fraud and scams. Attempts to scam residents can be made over the phone, by mail or email, through the internet or in person.  It is important to remain alert.  If an offer sounds too good to be true, it should be questioned.  To find out if a potential contractor is licensed to work in Louisiana, check the website of the Louisiana State Licensing Board for Contractors at \\online{L S L B C dot louisiana dot G O V}, or call them at (225) 765-2301.",
    copresent = "FEMA, the Louisiana Governor’s Office of Homeland Security and Emergency Preparedness",
    start = "2021-07-12",
    alert = "2022-05-15"
  )

  Spot(
    "BusinessStormPrepGovernor",
    StormPrep,
    "Is your business ready to recover after the damaging winds and floods of a hurricane? The Governor's Office of Homeland Security and Emergency Preparedness has a checklist for businesses so that they can stay in business despite a storm. Their web site is \\online{get a game plan dot O R G}. ",
    start = "2021-06-22",
    alert = "2022-07-15"
  )

  Spot(
    "Evacuteer",
    StormPrep,
    "Thousands of New Orleanians may need help evacuating this hurricane season.  You can assist by volunteering with Evacuteer, to help get people out of harm's way in case of an evacuation.  Evacuteer hosts one-hour volunteer trainings at libraries and community spaces throughout the city.  \\MoreWeb[More information, and registration for training, are]{evacuteer dot O R G} That's \\online{E V A C U T E E R dot O R G}.  ",
    start = "2021-06-22",
    alert = "2022-07-15",
    groupGainMultiplier = 1.2
  )

  Spot(
    "EvacuteerService",
    StormPrep,
    "New Orleans' City-Assisted Evacuation provides residents with a free ride to a shelter during a mandatory evacuation. They also provide a ride home once it is safe to return. Evacuteer volunteers will help residents and make sure that families, including pets, stay together.  \\MoreWeb{ready dot nola dot gov} ",
    start = "2021-06-22",
    alert = "2022-07-15",
    groupGainMultiplier = 1.5
  )

  Spot(
    "HurricaneContraflowAlertB",
    StormPrep,
    "Do you know the way out of town in a hurricane evacuation?  When a storm is approaching, our interstate highways are rerouted for \\emph{contraflow}, where all lanes head away from the coast.  During contraflow it's important to know where you're going, because there may be few exits from the highway once you enter.  More information about contraflow routes is available on the Evacuation Guide for Southeast Louisiana, available from the Governor's Office of Homeland Security and Emergency Preparedness website, \\textsl{get a game plan dot O R G}.",
    start = "2021-06-22",
    alert = "2022-07-15",
    groupGainMultiplier = 1.2
  )

  Spot(
    "HurricaneDisabilitiesB",
    StormPrep,
    "People with disabilities can reduce the fear, panic, and inconvenience that comes with hurricane evacuation by planning ahead.   The website of the Governor's Office of Homeland Security and Emergency Preparedness, at \\online{get a game plan dot O R G}, has a list of precautions and preparations.",
    start = "2021-06-22",
    alert = "2022-07-15",
    groupGainMultiplier = 1.2
  )

  Spot(
    "HurricanePetsAlertB",
    StormPrep,
    "You may have a hurricane evacuation plan, but can your pet come along?   The website of the Governor's Office of Homeland Security and Emergency Preparedness has information about planning for your pets as a storm approaches.  \\MoreWeb{get a game plan dot O R G}",
    start = "2021-06-22",
    alert = "2022-07-15",
    groupGainMultiplier = 1.2
  )

  Spot(
    "HurricaneShelterAlert",
    StormPrep,
    "Do you have somewhere to go when a hurricane approaches?  Just because you can't afford a hotel, doesn't mean you have to ride out a storm.  For information about shelter options, get the Evacuation Guide for Southeast Louisiana from the Governor's Office of Homeland Security and Emergency Preparedness.  You can download it from their website, \textsl{get a game plan dot O R G}.",
    start = "2021-06-22",
    alert = "2022-07-15",
    groupGainMultiplier = 1.2
  )

  Spot(
    "SEfloodProtectWebsiteB",
    StormPrep,
    "The Southeast Louisiana Flood Protection Authority-West, which provides flood protection for nearly all land on the west bank of the Mississippi River in Jefferson and Orleans parishes, provides information to the public via its web site, \\online{W W W dot S L F P A W dot O R G}. The site provide information to the public about flood incidents, ongoing projects, and permit guidelines. Again, the URL is \\online{W W W dot S L F P A W dot O R G}.",
    start = "2021-06-22",
    alert = "2022-07-15"
  )

  Spot(
    "StormPrepCityWebSiteE",
    StormPrep,
    "NOLA Ready is the City of New Orleans's online Emergency Alert System.  Its web site \\online{ready dot NOLA dot G O V} helps the citizens of New Orleans get themselves, their homes and their businesses prepared for hurricane season. Information includes supplies citizens should have on hand, how to form an emergency plan, what to include in an emergency kit and to-go bag, how to get out of town, and resources for pet-owners and business owners. In the event of emergency, \\online{ready dot NOLA dot G O V} will provide fast, accurate updates.",
    start = "2021-06-22",
    alert = "2022-07-15",
    groupGainMultiplier = 1.65
  )

  Spot(
    "StormTaxPrepIRS", StormPrep,
    "The IRS offers resources to help both individuals and businesses prepare for the disruption a hurricane can bring. Their disaster loss workbooks can help you compile a room-by-room list of personal belongings and business equipment. Those workbooks and more information are available on their website at \\online{I R S dot G O V slash publications slash p 584} and {publications slash p 584 b}.",
    start = "2021-06-22",
    alert = "2022-07-15"
  )

  Spot(
    "TaxPrepElectronicRecordsIRS",
    StormPrep,
    "The IRS encourages you to be prepared for the year's hurricane season. Electronic records are an easy way to make your financial history portable. You can scan important records such as W-2s, tax returns and other paper documents onto an electronic format, and carry them  on a CD or DVD or other electronic storage device.You can also photograph or videotape the contents of your home and/or business, especially items of greater value, for tax and insurance purposes. Remember to store these electronic records away from areas at risk, or to bring them with you when you evacuate.",
    start = "2021-06-22",
    alert = "2022-07-15"
  )

  Spot(
    "HolidayPetChocolate",
    Holiday,
    "Holiday cooking and gifts often include chocolate.  However, chocolate is very toxic to pets and can cause digestive, heart and brain disease including vomiting, rapid heart rate, high blood pressure, and seizures.  Exposure to chocolate can be life-threatening to pets, and treatments are expensive.  The best way to protect pets is to limit their risk of exposure at home by making sure that chocolate snacks are out of their reach. More information about poisons and pets is available from the ASPCA's web site, \\online{A S P C A  dot O R G slash pet hyphen care slash animal hyphen poison hyphen control}~.",
    sourceContacts = "Ginger Guttner <ginger@lsu.edu>",
    start = "2021-06-22",
    alert = "2022-07-15",
    copresent = "LSU School of Veterinary Medicine"
  )

  Spot(
    "HolidayPetElectricity",
    Holiday,
    "Holiday decorations often involve more electrical cords, and pets may be attracted to them.  Electric shock from chewed cords may have devastating consequences.  Pet owners can try to reduce pets' access to cords, watch pets to make sure they are not chewing on new cords.  Some pets may try to eat batteries, which should also be stored safely out of their reach.  LSU's Veterinary Teaching Hospital will remain open 24 hours daily through the holidays. \\MorePhoneWeb{225/578-9600}{L S U dot E D U slash vet med}",
    sourceContacts = "Ginger Guttner <ginger@lsu.edu>",
    start = "2021-06-22",
    alert = "2022-07-15",
    copresent = "LSU School of Veterinary Medicine"
  )

  Spot(
    "HolidayPetWrappers", Holiday,
    "Candy wrappers, aluminum foil, plastic wrap, or ribbons can lead to serious problems if eaten by dogs or cats. Tinsel is particularly enticing to cats.  When ingested in sufficient quantities, it binds into a rope that can cause severe intestinal obstruction and require surgical treatment. Any small decoration or toy poses a swallowing hazard. If a child can choke on small toys or parts, then so can the family dog or cat.  LSU's Veterinary Teaching Hospital will remain open 24 hours daily through the holidays. \\MorePhoneWeb{225/578-9600}{L S U dot E D U slash vet med}",
    sourceContacts = "Ginger Guttner <ginger@lsu.edu>",
    start = "2021-06-22",
    alert = "2022-07-15",
    copresent = "LSU School of Veterinary Medicine"
  )

  Spot(
    "HolidayPetTableFood",
    Holiday,
    "Table food can cause dogs to suffer from acute gastroenteritis (``GAS-tro-En-ter-Eye-tiss'') or pancreatitis (``PAN-kree-uh-Tie-tiss''). In both diseases, dogs experience severe vomiting, diarrhea, abdominal pain, and listlessness. Bones may obstruct the esophagus, the stomach or the intestine and lead to severe digestive symptoms. Furthermore, grapes, raisins and onions are foods that dogs and cats should not receive. They are toxic to pets and can cause potentially fatal diseases.  Pet owners can avoid these risks by making sure that they and their guests do not feed table food to pets.  More information about poisons and pets is available from the ASPCA's web site, \\online{A S P C A dot O R G slash pet hyphen care slash animal hyphen poison hyphen control}~.",
    sourceContacts = "Ginger Guttner <ginger@lsu.edu>",
    start = "2021-06-22",
    alert = "2022-07-15",
    copresent = "LSU School of Veterinary Medicine"
  )

  Spot(
    "HolidayPetPlants",
    Holiday,
    "Over the holidays, many of us will have seasonal plants in the house.  But ornamental plants like poinsettias (``point-SET-uhz''), mistletoe and holly are toxic to animals.  They can cause upset stomachs or worse symptoms in dogs and cats.  Owners can protect pets by placing them away from where pets go, and by watching for signs that the plants are being eaten.  More information about poisons and pets is available from the ASPCA's web site, \\online{A S P C A dot O R G slash pet hyphen care slash animal hyphen poison hyphen control}~.",
    sourceContacts = "Ginger Guttner <ginger@lsu.edu>",
    start = "2021-06-22",
    alert = "2022-07-15",
    copresent = "LSU School of Veterinary Medicine"
  )

  Spot(
    "HolidayPetFreeze",
    Holiday,
    "The weather in December and January can be quite chilly even in Louisiana. Please remember to bring in outside pets overnight if a hard freeze is forecast.",
    sourceContacts = "Ginger Guttner <ginger@lsu.edu>",
    start = "2021-06-22",
    alert = "2022-07-15",
    copresent = "LSU School of Veterinary Medicine"
  )

  Spot(
    "NationalSuicidePrevCarnival",
    Carnival,
    "The National Suicide Prevention crisis counseling hotline is 800/273-8255.  This number is available both for people who are having suicidal thoughts, and who are concerned about others.  That number again is 800/273-8255.",
    start = "2021-06-22",
    alert = "2022-07-15",
    boost = 0.45
  )

  Spot(
    "RapeCrisisCarnival",
    Carnival,
    "The Greater New Orleans Center for Women and Children operates programs to assist victims of domestic violence and sexual assault. Services include a 24 hour confidential crisis line, shelter for victims of domestic violence, limited legal assistance (including referrals), and counseling and advocacy services.  All services are confidential and are provided at no cost by specially trained staff members.  Help is provided both for victims, and for people who want to stop committing violent acts.  Assistance is available by calling 504/837-5400.",
    start = "2021-06-22",
    alert = "2022-07-15",
    boost = 0.54
  )

  Spot(
    "SAPHEcarnival",
    Carnival,
    "SAPHE (``safe''), Tulane's Sexual Aggression Peer Hotline and Education program, provides the Tulane community with resources, support and education about sexual aggression. If you or someone you know is a victim of sexual assault, relationship violence, stalking, harassment, or exploitation, you can call SAPHE's student-operated, confidential hotline at 504/654-9543. Again, the hotline's number is 504/654-9543.",
    start = "2021-06-22",
    alert = "2022-07-15",
    boost = 0.45
  )

  Spot(
    "CityParadeParking",
    Carnival,
    "The City of New Orleans has special restrictions for parking near parades and for using neutral grounds during carnival season. \\MoreWeb{nola dot G O V}",
    start = "2021-06-22",
    alert = "2023-01-15"
  )

  Spot(
    "SummerInjuryPrevention",
    Summer,
    "Forty percent of all injury-related emergency room visits --- and forty-two percent of all injury deaths --- happen between May and August.  Tips on how to keep your children active and safe over the summer are available online at \\online{H L C online dot O R G}.",
    start = "2021-06-22",
    alert = "2022-07-15"
  )

  Spot(
    "DrowningPrevention",
    Summer,
    "Drowning is a leading cause of accidental death for children under fifteen. Parents should know CPR, have a life preserver on hand, and never let children swim in a pool unsupervised. More summer water safety tips are available online at \\online{H L C online dot O R G}.",
    start = "2021-06-22",
    alert = "2022-07-15"
  )
}

/** Seasonal scheduling rules for long-term PSAs. */
object PsaScheduling extends AssortmentSchedule, Utils.Converters {
  import scala.language.implicitConversions
  import Group.*
  import Assortment.*

  /** Default assortment when nothing else applies */
  private val baseWeights = Map(
    Volunteer -> MED_LOW_GAIN,  Health -> MED_LOW_GAIN,
    Mental -> LOW_GAIN, Services -> LOW_GAIN,
    Voter -> NO_GAIN, Civic -> NO_GAIN,
    Eco -> NO_GAIN,    Animal -> NO_GAIN, Edu -> NO_GAIN,
    TaxAlways -> NO_GAIN, Museum -> NO_GAIN,
    Rare -> NEG_GAIN
  )

  // The default
  Assortment(LocalDate.MIN, always, baseWeights, LocalDate.MAX)

  Assortment("2021-01-01", isJanuary,
    baseWeights + (Taxtime -> MED_HIGH_GAIN) - TaxAlways + (Carnival -> HIGH_HIGH_GAIN))
  Assortment("2021-02-01", isFebruary,
    baseWeights + (Taxtime -> HIGH_GAIN) - TaxAlways)
  Assortment("2021-03-01", isMarch,
    baseWeights + (Taxtime -> MED_HIGH_GAIN) + (Voter -> HIGH_GAIN) - TaxAlways)
  Assortment("2021-04-01", isApril,
    baseWeights + (Taxtime -> MED_HIGH_GAIN) + (Voter -> HIGH_GAIN) - TaxAlways)
  Assortment("2021-05-01", isMay, baseWeights + (Summer -> HIGH_GAIN))
  Assortment("2021-06-01", isJune,
    baseWeights + (Summer -> HIGH_GAIN) + (StormPrep -> HIGH_HIGH_GAIN))
  Assortment("2021-07-01", isJuly,
    baseWeights + (Summer -> MED_HIGH_GAIN) + (StormPrep -> HIGH_GAIN))
  Assortment("2021-08-01", isAugust,
    baseWeights + (StormPrep -> MED_HIGH_GAIN) + (Voter -> MED_HIGH_GAIN))
  Assortment("2021-09-01", isSeptember, baseWeights + (Voter -> HIGH_GAIN))
  Assortment("2021-10-01", isOctober, baseWeights + (Voter -> HIGH_GAIN))
  Assortment("2021-11-01", isNovember, baseWeights)
  Assortment("2021-12-01", isDecember,
    baseWeights - Services + (Services -> HIGH_HIGH_GAIN)
      - Health + (Health -> HIGH_HIGH_GAIN)
      - Mental + (Mental -> HIGH_HIGH_GAIN)
      + (Holiday -> HIGH_HIGH_GAIN))
}
