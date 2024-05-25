// PSAs.scala --- (c) 2021 John Maraist
// Part of the WTUL Roster Generator
//
// This file is made available under the GNU GPL version 3; see the
// LICENSE file in this distribution or https://fsf.org/ for more
// details.

// To skip to the actual PSA spots: search for "short-term" or
// "long-term".

// Phonemes: https://cloud.google.com/text-to-speech/docs/phonemes

package org.maraist.wtulrosters
import java.time.{LocalDate, LocalDateTime, LocalTime}
import java.time.format.DateTimeFormatter
import org.maraist.structext.{SpeakAs, StructText, fromString}
import org.maraist.structext.StructText.*

object PSAutils {
  def basePolicy(slot: Int): Int =
    if slot < 14 then 3 else
      if slot < 29 then 5 else
        if slot < 46 then 3
          else 5
}

/** How we generate PSA rosters. */
object PsaRosters extends RosterType("psa-") {

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
    extends RosterBuilder("PSA", startDate, 78, PsaScheduling,
      "WTUL 91.5\\textsc{fm} --- PSA roster",
      "PSA \\#",
      "Please report typos, expired spots, or other problems with PSAs to \\textsl{wtul-psa@gmail.com}\\,.",
      (x: Int) => x.toString(),
      commonPreamble("PSAs"),
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
      ),
      Array[Array[Int]](
        Array[Int](0, 38, 46, 49, 52, 14, 55, 7, 0, 58, 28, 21, 42, 33, 7, 14, 28, 0, 61, 21, 67, 7, 70, 73),
          Array[Int](52, 1, 64, 70, 38, 46, 8, 1, 33, 15, 49, 8, 67, 29, 22, 42, 15, 34, 73, 8, 1, 55, 22, 58),
        Array[Int](16, 61, 23, 74, 56, 68, 2, 64, 9, 29, 39, 16, 47, 71, 2, 50, 9, 30, 23, 43, 9, 53, 2, 34),
        Array[Int](3, 35, 71, 43, 50, 24, 59, 10, 53, 17, 47, 3, 62, 24, 35, 10, 39, 17, 3, 68, 30, 10, 74, 65),
        Array[Int](72, 11, 69, 59, 65, 44, 36, 4, 25, 40, 11, 48, 18, 31, 62, 4, 56, 11, 25, 51, 18, 76, 4, 31),
        Array[Int](19, 5, 57, 77, 54, 63, 26, 66, 12, 32, 45, 26, 36, 5, 40, 69, 12, 48, 19, 60, 12, 5, 37, 75),
        Array[Int](13, 51, 6, 72, 76, 20, 57, 54, 27, 75, 6, 66, 32, 13, 45, 20, 60, 6, 37, 27, 63, 13, 41, 77)),
      true,
      "yellow"
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

  override def rosterSlotDateTime(roster: Roster, day: Int, idx: Int):
      LocalDateTime =
    LocalDateTime.of(roster.startDate.plusDays(day), LocalTime.of(idx, 30, 0))

  override val rosterDayCount: Int = 7

  override def rosterDaySlotCount(dayIdx: Int): Int = 24
}

/** Bank holding short-term PSAs. */
object PsaShortTermSpots extends SpotBank("psa-short", PsaScheduling) {
  import Group.*
  import scala.language.implicitConversions

  Event(
    "NovasMay2024",
    // Volunteer,
    str("Have you or a loved one been impacted by incarceration? Do you want to make a difference in the city's criminal justice system? New Orleans Voices for Accountability, or NOVAS, is now welcoming applications for new volunteer members. NOVAS is a community-led organization pursuing racial justice in the criminal legal system, and advocating for a reduction of New Orleans' jail population.") +
      moreWebEmail(
        "nola voices dot O R G", "info at nola voices dot O R G"),
    "2024-06-30",
    givenStart = Some("2024-05-27"),
    // "2024-05-27",
    // end = Some("2024-06-30"),
    // alert = "2024-06-20",
    spotsSourceContacts = Seq(
      "Devorah Levy-Pearlman <info@nolavoices.org>")
  )

  Event("KomenkaJune2024",
    str("The forty-second spring concert of the Komenka Ethnic Dance and Music Ensemble will be held")
      + blank("when")
      + str("at 7:30") > pm + str ("and Sunday the 2nd at 2") > pm > period
      + str("Both performances are at Loyola University's Louis J. Roussel Performance Hall, at 6301 Saint Charles Avenue.  The show is an ``around the world'' tour through ethnic dance and music, featuring presentations representing Appalachia, Cajuns, Estonia, Finland, Greece, Hawaii, India, North Macedonia, Poland, Portugal, Scotland, Slovakia, Tibet, Turkey and the American 1940's Jazz Period. Tickets are available online at")
      + online("ticket tailor dot com slash events slash komenka") > period
      + morePhoneEmail("504/529-4676", "R O D I J at copper dot net"),
    "2024-06-01",
    givenStart = Some("2024-04-01"),
    spotsSourceContacts = Seq("musaica@musaica.org")
  )

  Event("PublicHealth2024Apr22",
    str("In honor of Earth Day, the Tulane School of Public Health and Tropical Medicine will launch its Dean's Lecture Series on Climate Change and Health in the Gulf South")
      + blank("when.")
      + str("The invited guest, a professor from Columbia University, will discuss how climate change and its affects are impacting health in our region. Refreshments will be provided starting at 11:30") > am > comma + str("and discussion will begin at noon, in the Tidewater auditorium on the downtown campus at 1440 Canal Street")
      + moreWeb("S P H dot tulane dot E D U"),
    "2024-04-22",
    givenStart = Some("2024-04-03"),
    spotsSourceContacts = Seq("Dee Boling <dboling@tulane.edu>")
  )

  Event("PublicHealth2024Apr2",
    str("Tulane's School of Public Health and Tropical Medicine will host")
      + emph("Climate Change and Health in the Gulf South: A Symposium on Community, Research, Design, and Action") > period
      + str("The symposium will be")
      + blank("when")
      + str("in the Tidewater auditorium on the downtown campus, 1440 Canal Street, from 4:30") > pm + str("to 8") > pm > period
      + moreWeb("S P H dot tulane dot E D U"),
    "2024-04-02",
    givenStart = Some("2024-04-01"),
    spotsSourceContacts = Seq("Dee Boling <dboling@tulane.edu>")
  )

  Event("ApprenticeshipSept2023",
    str("Are you a Tulane affiliate or Loyola student with an interest in the weird and wonderful world of college radio? WTUL is accepting applications for apprenticeships. You can learn how to broadcast progressive music from the basement of the LBC, under the guidance of WTUL DJs. The application is on our website,")
      + online("W T U L dot F M") > comma
      + str("and is due")
      + blank("when.")
      + str("More information is available by email from our Apprentice Directors at")
      + online("W T U L dot apprentice at gmail dot com") > period,
    "2023-09-15",
    givenStart = Some("2023-09-11"),
    spotsSourceContacts = Seq("Jess Quint <jquint@tulane.edu>")
  )

  Event("MusaicaApr2024",
    str("Musaica concludes its 2023-24 season with a program entitled")
      + emph("The Soul of Music") > comma
      + str("featuring works of Florence Price, William Grant Still, Michel Damase, Dave Anderson, and Mollie O'Meara.")
      + str("Concerts will take place")
      + blank("when")
      + str("at the UNO Performing Arts Center Recital Hall, and on Tuesday the 23rd at the Saint Charles Avenue Presbyterian Church in New Orleans.   Both shows are at 7:30") > pm
      + moreWeb("musaica dot O R G"),
    "2024-04-22",
    givenStart = Some("2024-04-01"),
    spotsSourceContacts = Seq("musaica@musaica.org")
  )

  Event("MusaicaJan2024",
    str("Musaica continues its 2023-24 season with a program entitled")
      + emph("Fantasia Romantica") > comma
      + str("featuring works of Rota and Enescu.")
      + str("Concerts will take place")
      + blank("when")
      + str("at Saint Martin Episcopal Church in Metairie, and on Tuesday the 30th at the Saint Charles Avenue Presbyterian Church in New Orleans.   Both shows are at 7:30") > pm
      + moreWeb("musaica dot O R G"),
    "2024-01-29",
    givenStart = Some("2024-01-03"),
    spotsSourceContacts = Seq("musaica@musaica.org")
  )

  Event("MusaicaSept2023",
    str("Musaica begins its 2023-24 season with a program entitled")
      + emph("Heart of Genius") > comma
      + str("featuring works of Mozart, Beethoven, Brod and Moszkovski.")
      + str("Concerts will take place")
      + blank("when")
      + str("at Saint Martin Episcopal Church in Metairie, and on Tuesday the 12th at the Saint Charles Avenue Presbyterian Church in New Orleans.   Both shows are at 7:30") > pm
      + moreWeb("musaica dot O R G"),
    "2023-09-11",
    spotsSourceContacts = Seq("musaica@musaica.org")
  )

  Event("BocaflojaAug2023",
    str("GulfRoots Collective will host a potluck dinner and bilingual Spanish/English screening of the documentary")
      + emph("EnClave")
      + str("by the artist and filmmaker Bocafloja.  The event is open to the public, and will be at 6:30")
      > pm
      + blank("when")
      + str("at the First Unitarian Universalist Church, 2903 Jefferson Avenue.")
      + moreWeb("bit dot L Y slash boca hyphen nola"),
    "2023-08-03",
    spotsSourceContacts = Seq("Zach Kopkin <zach@gulfroots.co>")
  )

  Event("AutismHalloween2022",
    str("The Autism Society of Greater New Orleans is hosting their annual Inclusive Halloween Party")
      + blank("when,")
      + str("from 11") > am
      + str("to 2") > pm > period
      + str("The event will be at the Westwego Farmers Market, 484 Sala Avenue, with trick-or-treating, autism resources, and fun.  The event will be inclusive of all dietary restrictions, sensory needs, and ages of trick or treaters.")
      + moreWeb("A S G N O dot O R G slash halloween"),
    "2022-10-29",
    spotsSourceContacts = Seq("Alicia Dardar <eventcoordinator@asgno.org>")
  )

  Event("ReliefSpendingMeeting",
    str("The Mayor of New Orleans is hosting a Community Meeting to get input from the residents of New Orleans about how they think the $388 Million in economic relief dollars, allocated to the City of New Orleans, should be spent.  The meeting is")
      + blank("when,")
      + str("from 6 to 7") > pm
      + str("at L.B. Landry High School, 1200 B. Landry Avenue, on the Westbank. More information is available on the Facebook and Instagram pages of the New Orleans Workers’ Center for Racial Justice."),
    "2022-10-05",
    spotsSourceContacts = Seq("Austen Angers <austen@all4energy.org>")
  )

  Event("PSCforum2022a",
    str("The Louisiana Public Service Commission has two seats up for election this year, including here in District 3.  The Alliance for Affordable Energy is hosting a series of LPSC forums across the state, with a focus on voter education.  Their New Orleans forum will be")
      + blank("when,")
      + str("at the Lutcher Library, from six to eight p.m.  Candidates for this seat will speak and answer questions.  More information is available on the Alliance for Affordable Energy's web site,")
      + online("all 4 energy dot O R G slash L P S C dot H T M L")
      > comma + str("that's")
      + online("all 4 energy")
      + str("with the digit ``four.''"),
    "2022-09-21",
    spotsSourceContacts = Seq("Austen Angers <austen@all4energy.org>")
  )

  Event("autismSchollSupply2022",
    str("The Autism Society of Greater New Orleans and the Jefferson Parish Recreation Department will distribute school supplies and COVID-19 vaccines at two Back to School Events")
      + blank("when,")
      + str("one in Terrytown and one on the east bank.  The West Bank location is the Terrytown Playground Gym, 641 Heritage Avenue.  The East Bank location is the Ree-Alario Special Needs Center at 6900 Saints Drive in Metairie.  Each child aged six and up will receive one mesh backpacks with school supplies.  COVID-19 vaccines will be available for kids aged six months and up.  The event is open to the public, but an online signup is required to attend, at")
      + online("A S G N O dot O R G slash back hyphen to hyphen school")
      > period,
    "2022-07-23",
    spotsSourceContacts = Seq("FEMA-NewsDesk-Louisiana-Disasters@fema.dhs.gov")
  )

  Event("sierraMtgJune22",
    str("The next meeting of the Orleans Sierra Club will be") +
      blank("when") +
      str("at 6:30") + pm > period +
      str("Zach Kopkin, the Sierra Club's regional organizer, will discuss the Sierra Club’s ``Beyond Dirty Fuels'' Campaign.  The campaignaims to stop the expansion of dirty fossil fuels infrastructure projects, bring about a safer climate future, and help open a path for clean, renewable energy.  "
        + "  The meeting will be at First Unitarian Universalist, 5212 South Claiborne.") +
      moreWebEmail("sierra club dot O R G slash delta", "doc tim sierra at gmail dot com"),
    "2022-06-12",
    spotsSourceURL = Seq("https://www.sierraclub.org/delta/new-orleans-group")
  )

  Event("FemaAppealsIda",
    str("If you received a letter from FEMA saying that the information you provided is incomplete, or that you are ineligible for disaster assistance, you have the right to appeal the decision within sixty days of the date on the letter, or longer with an explanation for the delay.  An appeal letter from you may change FEMA’s decision.  If your FEMA letter says your request was denied because of missing information, providing the necessary documentation may help you qualify for a grant.  Or, if you don’t agree with the amount of the grant you received, providing receipts or written quotes may allow you to receive a larger grant.  More information about the FEMA appeal process is available on their website") > comma + online("F E M A dot G O V") > comma + str("searching for the word `appeal'") > comma + online("A P P E A L") > period,
    "2022-03-31",
    spotsSourceContacts = Seq("FEMA-NewsDesk-Louisiana-Disasters@fema.dhs.gov")
  )

  Event("TUbookfair2022",
    str("The New Orleans Book Festival at Tulane University will debut ") + blank("when,") + str(" on Tulane’s uptown campus, and run through Saturday the 12th. The event is open to the public and will feature over 130 authors.") + moreWebEmail("bookfest dot tulane dot E D U", "bookfest at tulane dot E D U"),
    "2022-03-10",
    spotsSourceContacts = Seq(
      "Christopher R Dunaway <roger@tulane.edu>",
      "Brendan F Chase <bchase2@tulane.edu>")
  )

  Event("IdaFemaIndivLastRegister",
    str("Survivors of Hurricane Ida still have two weeks left to apply for individual assistance from FEMA. The final deadline to apply is") + blank("when.") + str("FEMA assistance for individuals and families can cover rental assistance, temporary housing, home repairs, personal property losses and other disaster-related needs not covered by insurance.  The easiest way to apply for FEMA assistance is online at") + online("disaster assistance dot gov") > comma + str("or by phone at 800-621-3362.  Phone lines operate from 6") + am + str("to midnight, seven days a week. Individuals can also visit a Disaster Recovery Center to apply and meet with FEMA specialists in person.  Center locations are available on the FEMA app, or online at") + online("fema dot gov slash D R C Locator") > period,
    "2021-11-29",
    spotsSourceContacts = Seq("FEMA-NewsDesk-Louisiana-Disasters@fema.dhs.gov")
  )

  Event("MusicaSept2022",
    str("Musica Chamber Ensemble starts its seventeenth season with") +
      doublequoted(str("A Musical Family")) > comma +
      str("a program of beautiful pieces from the Mendelssohn siblings, with Fanny Mendelssohn's String Quartet in E flat major and Felix Mendelssohn's Second String Quintet, plus works by Johann Christian Bach and Samuel Coleridge-Tayor. The performances will take place") +
      blank("when") +
      str("at the Munholland Methodist Church on 1201 Metairie Road, and Tuesday the thirteenth at the Saint Charles Avenue Presbyterian Church, 1545 State Street.  Both performances are at 7:30pm and are open to the public with a suggested donation.") +
      moreWebPhone("musica dot O R G", "504/304-8608"),
    "2022-09-12",
    spotsSourceContacts = Seq("Bruce Owen <musaica@musaica.org>")
  )

  Event("MusicaSept2021",
    str("Musica Chamber Ensemble starts its season with") +
      doublequoted(str("A Musical Journey")) > comma +
      str("featuring the music of Amy Beach, Robert") +
      phonetic(str("Schumann"), "SHOE-mahn", "ˈʃuː.man") > comma +
      str("Akira Miyoshi, Ennio Morricone, Florence Price, and Max") +
      phonetic(str("Bruch"), "broohk", "ˈbRUx") > period +
      str("The performances will take place") +
      blank("when") +
      str("at the Saint Charles Avenue Presbyterian Church, and Tuesday the 14th at the") +
      spellout("UNO") +
      str("Performing Arts Center Recital Hall.  Both performances are at 7:30pm and are open to the public with a suggested donation.") +
      moreWebPhone("musica dot O R G", "504/304-8608"),
    "2021-09-13",
    spotsSourceContacts = Seq("Bruce Owen <bruceowen@aol.com>")
  )

  Event("MapleLeafVaxDayAug2021",
    str("The Maple Leaf will host a vaccination day at their bar") + blank("when,") + str("from 6 to ten") + pm + str("in cooperation with Castellon pharmacy.  The bar will be open, and there will be live music for your listening pleasure.  People will be offered their choice of either the single-dose Johnson-and-Johnson vaccine, or the two-dose Pfizer vaccine.  There is no charge for vaccination for COVID-19.  Again, the vaccination day will be") + blank("when,") + str("from six to ten") + pm + str("at the Maple Leaf Bar."),
    "2021-08-14",
    spotsSourceNote = "Facebook post in WTUL group."
  )

  Event("LionsScreeningsAug2021Cancelled",
    str("The") +
      doublequoted("Lions  Health Awareness Day,") +
      str("originally scheduled for the 28th, has been cancelled due to the COVID resurgence.  The Lions Club hopes to reschedule the event in early") +
      speak("2022", SpeakAs.Date("y")),
    "2021-08-28",
    spotsSourceContacts = Seq("Aida Grace <dajg@aol.com>")
  )

  Event("sierraMtgJuly21",
    str("The next meeting of the Orleans Sierra Club will be") +
      blank("when") +
      str("at 6:30") + pm > period +
      str("Christen Steele will discuss conservation issues affecting the survival of the monarch butterfly.  Sierra Club meetings are currently online.  More information and a link to the online session are available on their website,") +
      online("sierra club dot O R G slash delta") > period,
    "2021-07-21",
    spotsSourceURL = Seq("https://www.sierraclub.org/delta/new-orleans-group")
  )
}

/** Bank holding long-term PSAs. */
object PsaLongTermSpots extends SpotBank("psa-long", PsaScheduling) {
  import Group.*
  import scala.language.implicitConversions

  Spot(
    "AllOfUsResearch2022",
    Volunteer,
    str("The ``All of Us'' research program is investigating why some people get sick and others stay healthy. In 2022, they are seeking volunteers to participate by completing surveys, attending a clinic visit, and sharing health information.  In return, you can receive your genetic information.") +
      moreWebPhone("join all of us dot org slash Tulane", "504/988-0650"),
    start = "2022-03-06",
    end = Some("2022-12-31"),
    alert = "2022-12-05",
    orgName = Some("Tulane University School of Medicine"),
    sourceContacts = Seq(
      "Marie Towns <mtowns@tulane.edu>",
      "Emily Callegari <ecallegari@tulane.edu>")
  )

  Spot(
    "NewNeighborProjectOct2021",
    Services,
    str("The New Neighbor Project creates a holistic pathway to citizenship for refugees and immigrants in our community.   They provide English and citizenship classes, application assistance, and scholarship opportunities.") +
      moreWeb("the new neighbor project dot O R G"),
    start = "2021-11-01",
    alert = "2022-03-23",
    orgName = Some("New Neighbor Project"),
    sourceContacts = Seq("Amy Dudgeon <thenewneighborproject@gmail.com>")
  )

  Spot(
    "CrescentCareTestingAug21",
    Services,
    str("Crescent Care, formerly The NO/AIDS Task Force, offers no-cost rapid HIV testing. The test is quick and painless. It requires only a sample of oral fluid and results are available in as little as twenty minutes. You can be tested at their Saint") +
      phonetic("Roch", "rock", "ɹɑːk") +
      str("location, 1631") +
      phonetic("Elysian", "eh-LEE-zhun", "əˈliː.ʒən") +
      str("Fields, on Tuesdays from noon to five") +
      pm > comma +
      str("Wednesdays from ten") + am + str("to five") + (pm > period) +
      str("Thursdays from ten") + am + str("to three") + (pm > comma) +
      str("and Fridays from ten") + am + str("to five") + pm > period +
      str("Take-home HIV tests are also available five days a week from nine") +
      am + str("to five") + pm + str("at the same location.") +
      moreWeb("crescent care dot O R G"),
    orgName = Some("Crescent Care"),
    start = "2021-08-12",
    alert = "2023-01-23",
    previousAlerts = Seq("2022-02-20"),
    sourceContacts = Seq(
      "Doreen Tollerson <Doreen.Tollerson@crescentcare.org>",
      "Joe Hui <Joe.Hui@crescentcare.org>",
      "Lisa Mirman <lisamirman@gmail.com>"
    )
  )

  Spot(
    "VoteDotOrg",
    Volunteer,
    str("Do you need to register to vote, check your registration status, or find your polling place?  Do you want to volunteer to be an election site poll worker?")
      + sentence(
        str("You can find out more about voter and election information at")
          + online("vote dot O R G")
          > period)
      + str("Quick links will connect you to every state.  For your state of residence, you can register, check registration status, find your polling place, request an absentee ballot, or volunteer to be a poll worker.")
      + moreWeb("vote dot O R G"),
    orgName = Some("VoteDotOrg"),
    start = "2021-06-22",
    alert = "2023-03-23",
    sourceNote = "Local"
  )

  Spot(
    "HnocVolunteersTwelve",
    Volunteer,
    str("The Historic New Orleans Collection is a museum, research center, and publisher in the French Quarter. They are looking for weekend volunteers to help greet visitors, monitor exhibitions, and lead tours.") + moreWeb("H N O C dot O R G"),
    orgName = Some("Historic New Orleans Collection"),
    start = "2021-06-22",
    alert = "2021-11-01",
    end = Some("2021-10-30"),
    sourceContacts = Seq("wrc@hnoc.org")
  )

  Spot(
    "HnocVolunteersTwelve2",
    Volunteer,
    str("The Historic New Orleans Collection is a museum, research center, and publisher in the French Quarter. They are looking for weekend volunteers to help greet visitors, monitor exhibitions, and lead tours.") + moreWeb("H N O C dot O R G slash support slash volunteer"),
    orgName = Some("Historic New Orleans Collection"),
    start = "2021-11-01",
    alert = "2021-07-23",
    end = Some("2022-08-31"),
    previousAlerts = Seq("2021-10-27", "2021-11-13"),
    sourceContacts = Seq("wrc@hnoc.org")
  )

  Spot(
    "SharedHousingOfNOLATwo",
    Volunteer,
    str("Shared Housing of New Orleans matches people seeking places to live with elderly and disabled homeowners.  The home-seeker does light housekeeping, and stays on the premises at night. The home owner avoids nursing home or other assisted-living facilities by having this assistance and companionship.") + moreWebPhone("shared housing of new orleans dot O R G", "504/896-2575"),
    orgName = Some("Shared Housing of New Orleans"),
    start = "2021-06-22",
    alert = "2022-01-07",
    sourceNote = "confirmed July 2015 --- strauss@sharedhousingofneworleans.org (Marion Strauss)"
  )

  Spot(
    "InnocenceProjectGeneralThree",
    Volunteer,
    str("Innocence Project New Orleans frees innocent people sentenced to life in prison, and those serving unjust sentences. They work to expose and address the systemic racism and inequities at the root of wrongful convictions and unjust sentences, by sharing their clients’ stories in courts, and with the legislature, communities, and the media.") +
      phonetic("IPNO", "IPP-know", "ɪpˈnoʊ") +
      str("helps their clients to live well and fully in the world after their release.") +
      moreWebPhoneAnnounce(
        "Information about supporting IPNO is",
        "I P hyphen N O dot O R G",
        "504/943-1902"),
    orgName = Some("Innocence Project New Orleans"),
    start = "2021-06-22",
    alert = "2025-01-15",
    sourceContacts = Seq("info@ip-no.org", "Cat Forrester <catf@ip-no.org>"),
    sourceNote = "updated February 2022"
  )

  Spot(
    "InnocenceProjectGeneralTwo",
    Volunteer,
    str("Innocence Project New Orleans represents prisoners serving life sentences in Louisiana and Mississippi. They work to free innocent prisoners, prevent wrongful convictions, and assist freed prisoners with their transition upon release.") +
      moreWebPhoneAnnounce(
        "Information about supporting the Innocence Project is",
        "I P hyphen N O dot O R G",
        "504/522-4766"),
    orgName = Some("Innocence Project New Orleans"),
    start = "2021-06-22",
    alert = "2022-01-15",
    end = Some("2022-02-21"),
    previousAlerts = Seq("2022-02-20"),
    sourceContacts = Seq("info@ip-no.org", "Jene OKeefe Trigg <JeneOT@ip-no.org>"),
    sourceNote = "confirmed August 2015, updated February 2022"
  )

  Spot(
    "JeffParishAdoptParkwayTwo",
    Volunteer,
    str("Jefferson Parish's Adopt-a-Parkway Program started in") +
      date("1988", "y") > comma +
      str("Parkway sponsors improve landscape of their adopted sections of thoroughfares, and are recognized by a sign on their neutral ground.  More information is available on the Jefferson Parish website") +
      online("jeff parish dot net") > comma +
      str("or by phone at") + phone("504/349-5829") > period,
    orgName = Some("Adopt-a-Parkway Program"),
    start = "2021-06-22",
    alert = "2022-01-15",
    sourceNote = "updated from their website July 2015, added 9/2009"
  )

  Spot(
    "HabitatStTammanyReStore",
    Volunteer,
    str("Habitat for Humanity can make use of used housewares or surplus building materials.  Donated materials are made available for purchase at the Habitat for Humanity Re-Store, located on North Lane, off Highway 59, just north of I-12. Proceeds benefit Habitat for Humanity.") +
      morePhoneAnnounce(
        "More information including directions and volunteering is",
        "985/898-0642"),
    orgName = Some("Habitat for Humanity"),
    start = "2021-06-22",
    alert = "2022-01-15",
    sourceNote = "checked details online July 2015"
  )

  Spot(
    "CathCharitiesReadToKidsTwo",
    Volunteer,
    str("Catholic Charities seeks volunteers to read or tutor children and youth, work with seniors, and perform other duties.") +
      moreWebPhone("C C A N O dot O R G", "504/310-6960"),
    orgName = Some("Catholic Charities"),
    start = "2021-06-22",
    alert = "2022-01-28",
    sourceNote = "Pulled from Gambit, checked details online July 2015"
  )

  Spot(
    "ClearwaterWildlifeSanctuaryB",
    Volunteer,
    str("The Clearwater Wildlife Sanctuary seeks volunteers fostering abandoned young and rehabilitating injured animals and birds. Help is also needed at the Covington hospital site, the Madisonville educational center, and with transportating creatures. They also offer classes, including for skilled animal handling volunteers.") +
      moreWebPhone("clear water sanctuary dot O R G", "985/630-1009"),
    orgName = Some("Clearwater Wildlife Sanctuary"),
    start = "2021-06-22",
    alert = "2022-01-29",
    previousAlerts = Seq("2022-02-20"),
    sourceContacts = Seq("clearwaterwildlife@gmail.com"),
    sourceNote = "updated Feb. 2016"
  )

  Spot(
    "CommInSchoolsNolaGeneral",
    Volunteer,
    str("Communities In Schools of Greater New Orleans provides mentors in both Charter and Recovery District schools. Volunteer mentors meet with a child four hours every month to provide a positive role model.") +
      moreWebPhone("C I S new orleans dot O R G", "504/494-0328"),
    orgName = Some("Communities In Schools of Greater New Orleans"),
    start = "2012-08-01",
    alert = "2022-01-28",
    previousAlerts = Seq("2022-02-20"),
    sourceNote = "asked July 2015, last confirmed Dec. 2012",
    sourceContacts = Seq("Jade Parker <jparker@cisneworleans.org>")
  )

  Spot(
    "BirthingProjectGeneral",
    Volunteer,
    str("The New Orleans Birthing Project provides") + emph("sister friends,") +
      str("mentors for pregnant women of any age. They are seeking volunteers to be sister friends.") +
      moreWebPhoneAnnounce(
        "More information about becoming or having a sister friend is",
        "nola at birthing project U S A dot O R G", "504/482-6388"),
    orgName = Some("New Orleans Birthing Project"),
    start = "2021-06-22",
    alert = "2022-02-15",
    previousAlerts = Seq("2022-02-20"),
    sourceNote = "added Dec 2011, asked Dec. 2012, asked July 2015, asked at gmail address Feb 2022",
    sourceContacts = Seq("birthingproject@gmail.com",
      "bpusa@birthingprojectus.org",
      "nola@birthingprojectus.org")
  )

  Spot(
    "BoysHopeGirlsHope",
    Volunteer,
    str("Boys Hope Girls Hope intervenes on behalf of children in need between the ages of ten and eighteen, and provides them with a stable home, positive parenting, high-quality education, and the support needed to reach their full potential. They welcome both volunteers and donations.") +
      moreWeb("B H G H nola dot O R G"),
    orgName = Some("Boys Hope Girls Hope"),
    start = "2021-06-22",
    alert = "2022-02-15",
    previousAlerts = Seq("2022-02-20"),
    sourceContacts = "neworleans@bhgh.org"
  )

  Spot(
    "FlashFloodsGeneral",
    Volunteer,
    str("Flash flooding occurs when rain falls too quickly and for too long for the ground to absorb all of it.  A flash flood watch means conditions are favorable for a flash flood.  When a flash flood watch is issued, be alert to signs of flash flooding be ready to evacuate, don’t park your vehicle along bayous and canals, avoid low areas, and look out for rapidly rising water. A flash flood warning means that a flash flood is taking place or is imminent. When a flash flood WARNING is issued for your area, or the moment you realize that a flash flood is imminent, act quickly to save yourself.· Go to higher ground if possible.· Avoid places that are already flooded and anywhere water is moving rapidly.· Do not attempt to cross flowing waterways.· If advised to evacuate, do so immediately.· Continue monitoring weather forecasts and alerts--on radio, TV, or your phone.") +
      moreWebPhone("join all of us dot org slash Tulane", "504/988-0650"),
    start = "2022-09-12",
    alert = "2024-12-05",
    orgName = Some("FEMA"),
    sourceContacts = Seq(
      "Marie Towns <mtowns@tulane.edu>",
      "Emily Callegari <ecallegari@tulane.edu>")
  )

  Spot(
    "GreenLightVolunteersTwo",
    Volunteer,
    str("Volunteers with Green Light New Orleans install free energy-efficient light bulbs for any New Orleans resident.") +
      moreWebPhoneAnnounce(
        "More information about the program, and about becoming a volunteer, is",
        "green light new orleans dot O R G", "504/324-2429"),
    orgName = Some("Green Light New Orleans"),
    start = "2021-06-22",
    alert = "2022-02-15",
    previousAlerts = Seq("2022-02-20"),
    sourceNote = "(from Gambit) asked July 2015, asked Dec. 2012",
    sourceContacts = Seq(
      "green@greenlightneworleans.org (Bernnel January Jr.)",
      "Andreas Hoffmann <andreashoffmann@greenlightneworleans.org"
    )
  )

  Spot(
    "GiftOfLifeMarrowTwo",
    Volunteer,
    str("Gift of Life is a bone marrow and stem cell donor registry, helping children and adults with blood cancer find the matches they need, when they need them.") +
      moreWebPhone("gift of life dot O R G", "800/9-MARROW"),
    orgName = Some("Gift of Life"),
    start = "2021-06-22",
    alert = "2022-02-15",
    end = Some("2022-03-04"),
    previousAlerts = Seq("2022-02-20"),
    sourceNote = "last updated July 2015, added December 2014",
    sourceContacts = "Amy Glanzman <aglanzman@giftoflife.org>"
  )

  Spot(
    "GiftOfLifeMarrowThree",
    Volunteer,
    str("Gift of Life is a blood stem cell and bone marrow donor registry, helping children and adults battling blood cancer to find the matches they need, when they need them.") +
      moreWebPhone("gift of life dot O R G", "800/9-MARROW"),
    orgName = Some("Gift of Life"),
    start = "2022-03-05",
    alert = "2024-02-15",
    sourceContacts = "Amy Glanzman <aglanzman@giftoflife.org>"
  )

  Spot(
    "CASAJeffFour",
    Volunteer,
    str("Court-appointed special advocates help abused and neglected children to voice their needs in the legal process.") +
      moreWebPhoneAnnounce("More information, including volunteering to become a court-appointed special advocate for Jefferson Parish, is",
        "504/533-8757", "C A S A jefferson dot O R G."),
    orgName = Some("CASA"),
    start = "2021-06-22",
    alert = "2022-02-15",
    previousAlerts = Seq("2022-02-20"),
    sourceNote = "Feb 2022 tried the contact form on their site",
    // BOUNCES sourceContacts = Seq("Ellie Schneider <eschneider@casajefferson.org>")
  )

  Spot(
    "JeffersonAgingMealsOnWheels",
    Volunteer,
    str("The Jefferson Council on Aging seeks volunteers to deliver meals to homebound adults. Gas mileage will be reimbursed.") +
      morePhoneAnnounce(
        "More information and volunteer scheduling are", "504/888-5880"),
    orgName = Some("Jefferson Council on Aging"),
    start = "2021-06-22",
    alert = "2025-02-15",
    previousAlerts = Seq("2022-02-20"),
    sourceNote = "asked July 2015, last confirmed Feb 2022",
    sourceContacts = Seq("Chuck Sabin <csabin@jcoa.net>",
      "Al Robichaux <arobichaux@jcoa.net>", "Renee Schober <rschober@jcoa.net>")
  )

  Spot(
    "TheNewOrleansMissionShelterTwo",
    Volunteer,
    str("The New Orleans Mission, a Christian homeless shelter in Central City, is dedicated to serving the spiritual and physical needs of the poor, needy and homeless population of New Orleans. The mission provides meals, showers, clothing, shelter, literacy classes and job-skills training to men and women with children.") +
      morePhoneEmailAnnounce(
        "Information about volunteering or donating is",
        "504/415-9579", "brittany at new orleans mission dot O R G"),
    orgName = Some("New Orleans Mission"),
    start = "2021-06-22",
    alert = "2022-02-15",
    previousAlerts = Seq("2022-02-20"),
    sourceNote = "asked July 2015, last confirmed Dec. 2012.  Tried their web email form Feb 2022.",
    // BOUNCES sourceContacts = "Brittany Ray <Brittany@neworleansmission.org>"
  )

  Spot(
    "HabForHumanFive",
    Volunteer,
    str("Habitat for Humanity is in need of volunteers to achieve the goal of making decent, affordable housing a reality in the New Orleans area. No construction experience is necessary.  You can support them through volunteer labor or a donation.  More information is available by phone to Cynthia White at") +
      phone("504/861-4121") > period,
    orgName = Some("Habitat for Humanity"),
    start = "2021-06-22",
    alert = "2022-03-15",
    sourceNote = "Asked July 2015, confirmed Dec. 2012",
    sourceContacts = "communications@habitat-nola.org (Billy Wells)"
  )

  Spot(
    "Gyac",
    Volunteer,
    str("The Gulfsouth Youth Action") +
      phonetic("Corps", "core", "kɔːɹ") +
      str("seeks college student volunteers from all over the country to assist in providing recreational and educational opportunities for New Orleans-area inner-city youth and their families.") +
      moreWeb("the G Y A C dot O R G"),
    orgName = Some("Gulfsouth Youth Action Corps"),
    start = "2021-06-22",
    alert = "2022-03-15",
    sourceNote = "from Gambit, asked Dec. 2012, web site live",
    sourceURL = "http://thegyac.org/"
  )

  Spot(
    "NOLAoutreachArmsTwo",
    Volunteer,
    str("New Orleans Outreach seeks volunteers for their ARMS-Outreach after-school program. Volunteers are needed in the arts, academics, technology, recreation and life skills.") +
      moreWebPhone("N O outreach dot O R G", "504/654-1060"),
    orgName = Some("New Orleans Outreach"),
    start = "2021-06-22",
    alert = "2022-03-15",
    sourceNote = "asked for re-confirm Dec. 2012, web site live",
    sourceContacts = "bette@nooutreach.org"
  )

  Spot(
    "YMCAyesC",
    Volunteer,
    str("YMCA Educational Services, the adult literacy program of the YMCA of Greater New Orleans, offers adult education classes in reading, writing and math.  Classes are offered in Downtown New Orleans, Central City, and a new location in New Orleans East. Classes are open to the public, but since space is limited, registration is required.") +
      moreWebPhoneAnnounce("More information and volunteer signup are",
        "Y M C A new orleans dot O R G", "504/596-3842"),
    orgName = Some("YMCA Educational Services"),
    start = "2021-06-22",
    alert = "2022-03-15",
    sourceNote = "got update from them Jan 2014",
    sourceContacts = "Juliana Besenbruch <julieb@ymcaneworleans.org>"
  )

  Spot(
    "FreedomNotFreeOhNine",
    Volunteer,
    str("The") +
      doublequoted("Freedom Is Not Free") +
      str("program helps wounded and injured veterans of all branches of the military, and their families, defray the costs associated with care-giving and travel during their most trying times.") +
      moreWebAnnounce("More information about volunteering or donating is",
        "freedom is not free dot com"),
    orgName = Some("Freedom Is Not Free"),
    start = "2021-06-22",
    alert = "2022-03-15",
    sourceNote = "asked Dec. 2012, no answer but web site live June 2013",
    sourceContacts = "info@freedomisnotfree.com"
  )

  Spot(
    "StairGeneral",
    Volunteer,
    str("Start the Adventure in Reading, or STAIR, seeks volunteers to help second-grade public school students in the New Orleans area learn how to read.") +
      moreWebPhoneAnnounce("Information about tutor training sessions is",
        "www dot stair nola dot O R G", "504/899-0820"),
    orgName = Some("STAIR"),
    start = "2021-06-22",
    alert = "2022-03-15",
    sourceNote = "last confirmed Dec. 2012",
    sourceContacts = "Sara Woodard <swoodard@scapc.org>"
  )

  Spot(
    "RaintreeCFS",
    Volunteer,
    str("Foster parenting is an opportunity to make a difference in a child's life.  Raintree Children and Family Services offers training and a supportive network of staff to ease the transition for new foster parents.") +
      morePhoneAnnounce(
        "More information, including making a donation, is", "504/899-9045"),
    orgName = Some("Raintree"),
    start = "2021-06-22",
    alert = "2022-03-15",
    sourceNote = "asked Dec. 2012, no answer but web site live June 2013",
    sourceContacts = "Marlene Carter <mcarter@raintreeservices.org>"
  )

  Spot(
    "EqualityLouisiana",
    Volunteer,
    str("Equality Louisiana supports policy initiatives that will make Louisiana a better place for all families. They are currently seeking volunteers all across the state.") + moreWeb("equality L A dot O R G"),
    orgName = Some("Equality Louisiana"),
    start = "2021-06-22",
    alert = "2022-03-15",
    sourceNote = "added Dec. 2013",
    sourceContacts = "Micah Caswell <mcaswell@equalityla.org>"
  )

  Spot(
    "HagarHouseGeneral",
    Volunteer,
    str("Hagar's House provides temporary housing to women and their children in New Orleans as they seek more permanent homes. Hagar's House offers an open and empowering residential community, support for planning and budgeting, and a safe space for transition into sustainable housing. They welcome donations and volunteers from the New Orleans community.") + moreWebPhone("H A G A R S house nola dot O R G", "504/210-5064"),
    orgName = Some("Hagar's House"),
    start = "2021-06-22",
    alert = "2022-04-15",
    sourceNote = "asked Dec. 2012, no answer but web site live June 2013",
    sourceContacts = "hagarshouse@gmail.com"
  )

  Spot(
    "CASANolaFive",
    Volunteer,
    str("CASA New Orleans is seeking volunteers, especially African-American men, to serve as advocates for children in foster care.") + morePhoneEmail("504/522-1962", "info at C A S A new orleans dot O R G"),
    orgName = Some("CASA"),
    start = "2021-06-22",
    alert = "2022-04-15",
    sourceNote = "updated July 2015",
    sourceContacts = Seq(
      "Betsy Lopez <BLopez@casaneworleans.org>",
      "Mike Madej <mmadej@casaneworleans.org>"
    )
  )

  Spot(
    "HeartsOfChangeMentors",
    Volunteer,
    str("Hearts of Change Foundation provides support services to pregnant teens and teen parents.  They are seeking successful adults who were themselves teen parents to serve as mentors for pregnant teens.") +
      morePhoneEmail("504/621-8894", "hearts of change 2012 at gmail dot com"),
    orgName = Some("Hearts of Change"),
    start = "2021-06-22",
    alert = "2022-04-15",
    sourceNote = "added August 2013",
    sourceContacts = "gkannie95@yahoo.com (Kimberly Dilosa)"
  )

  Spot(
    "AidsPlanningCouncilVolunteersTwo",
    Volunteer,
    str("The New Orleans Regional AIDS Planning Council plans local HIV services to help ensure that everyone infected or affected by HIV can access treatment, and live full, productive lives.") +
      moreWebPhoneAnnounce(
        "More information about the council and about how you can volunteer is",
        "N O R A P C dot O R G", "504/821-7334"),
    orgName = Some("The New Orleans Regional AIDS Planning Council"),
    start = "2021-06-22",
    alert = "2022-04-15",
    sourceNote = "asked July 2015",
    sourceContacts = "brandi@norapc.org (Brandi Bowen)"
  )

  Spot(
    "TouroVolunteers",
    Volunteer,
    str("Touro Infirmary seeks adult volunteers to assist with the Family Surgery Lounge, Patient Information Desk, book and goody cart, hospital tours and health screenings.") + morePhone("504/897-8107"),
    orgName = Some("Touro"),
    start = "2021-06-22",
    alert = "2022-04-15",
    sourceNote = "(from Gambit)"
  )

  Spot(
    "RootsOfMusicGeneralTwo",
    Volunteer,
    str("The Roots of Music teaches, protects and empowers at-risk youth through music education, academic support and mentorship. At the same time, they help to preserve and promote the great musical and cultural heritage of New Orleans.") +
      moreWebAnnounce(
        "More information about events, donating or volunteering is",
        "the roots of music dot com"),
    orgName = Some("Roots of Music"),
    start = "2021-06-22",
    alert = "2022-04-15",
    sourceContacts = "elexa@therootsofmusic.com (Elexa, no last name given)",
    sourceNote = "asked July 2015"
  )

  Spot(
    "StompTroopers",
    Volunteer,
    phonetic("NOLArts", "NOL-arts") +
      str("Learning Center provides cultural access to young people with special needs.  Their flagship project, the STOMP Troopers, is an eight-week workshop to prepare young people with autism and other special needs to perform in the Chewbacchus Mardi Gras Parade.") +
      moreWebAnnounce("More information about donating or volunteering is", "stomp troopers dot O R G"),
    orgName = Some("NOLArts"),
    start = "2021-06-22",
    alert = "2022-05-15",
    sourceContacts = "Kate Lacour <nolartslearningcenter@gmail.com>"
  )

  Spot(
    "MusiciansClinicGeneralSupport",
    Volunteer,
    str("The New Orleans Musicians' Clinic has provided comprehensive health care and social services to thousands of local musicians, bearers of New Orleans traditions, service industry workers and many others since") +
      date("1998", "y") > period +
      str("You can support their mission through volunteering, or by donation.") +
      moreWeb("new orleans musicians clinic dot O R G"),
    orgName = Some("Musicians' Clinic"),
    start = "2021-06-22",
    alert = "2022-05-15",
    sourceContacts = "Kari Elgin <kari@nomaf.org>"
  )

  Spot(
    "LCMtoddlerTime",
    Edu,
    str("Every Thursday, the Louisiana Children's Museum hosts Toddler Time, activities for children ages 3 and under, and their parents or caregivers. Toddler Time is from ten to ten-thirty") + (am > period) + str("The museum is located at 15 Henry Thomas Drive in City Park.") + moreWebPhone("www dot L C M dot O R G", "504/523-1357"),
    orgName = Some("LCM"),
    start = "2021-06-22",
    alert = "2022-05-15",
    sourceNote = "(with other spots from Gambit)"
  )

  Spot(
    "ToastmastersGeneral",
    Edu,
    str("Toastmasters provides practice and feedback on public speaking, presentation, and communication skills.  Several groups regularly meet in the New Orleans area.") + moreWeb("toast masters dot O R G"),
    orgName = Some("Toastmasters"),
    start = "2021-06-22",
    alert = "2022-05-15"
  )

  Spot(
    "TeacherUnionWritingWorkshop",
    Edu,
    str("The United Teachers of New Orleans hosts monthly writing workshops.  The workshops are open to all New Orleans public school teachers and secondary school students.") + morePhone("504/304-2160"),
    orgName = Some("United Teachers"),
    start = "2021-06-22",
    alert = "2022-05-15",
    sourceContacts = "Sherri Wilder <SWilder@utno.org>",
    sourceNote = "still on in April 2010"
  )

  Spot(
    "LCMArtTrekGeneral",
    Edu,
    str("Art Trek is the Louisiana Children's Museum's working art studio for drawing, painting, sculpture, screen printing, collage and more.  Programs include walk-in classes, in-depth workshops, and weeklong camps.  The curriculum focuses on the history, aesthetics, production and appreciation of art,  thinking creatively, learning about artists and art forms, and creating personal works of art.") + morePhone("504/586-0725, extension 212"),
    start = "2021-06-22",
    alert = "2021-11-01",
    end = Some("2021-10-31"),
    sourceURL = "http://lcm.org/playlearn/art_programs.html",
    sourceNote = "(with other spots from Gambit)"
  )

  Spot(
    "LCMfamilyGames",
    Edu,
    str("Every Thursday, the Louisiana Children's Museum hosts Family Game Night, from four to six") + (pm > period) + str("The museum is located at 420 Julia Street.") + moreWebPhone("www dot L C M dot O R G", "504/523-1357"),
    start = "2021-06-22",
    end = Some("2021-06-22"),
    alert = "2022-05-15",
    sourceNote = "From Gambit"
  )

  Spot(
    "TeacherFreeStoreOctTwenty",
    Edu,
    str("The Teacher Free-Store provides quality school materials to local public school teachers.  They offer general classroom supplies, copying and printing equipment, and a lamination center.") + moreWeb("stem library lab dot O R G"),
    orgName = Some("Teacher Free-Store"),
    start = "2021-06-22",
    alert = "2022-05-15",
    sourceContacts = "Sean Moore <sean@stemlibrarylab.org>"
  )

  Spot(
    "FeedJusticeUUgeneral",
    Services,
    str("Every month, the Feed Justice Team of the First Unitarian-Universalist Church of New Orleans hosts fresh, healthy meals to build a stronger community and neighborhood.  The meals are open to all, reflect diverse culinary traditions, and are part of the team's food justice work.") + moreEmailAnnounce("More information about the meal, and about donating to support the meals, is", "feed justice at first U U N O dot O R G"),
    orgName = Some("First Unitarian-Universalist"),
    start = "2021-06-22",
    alert = "2022-05-15",
    sourceContacts = "Linda Reine <Linda@reine.biz>",
    sourceNote = "added August 2018"
  )

  Spot(
    "VeteransHousingOutreach",
    Services,
    str("Veterans Housing Outreach provides shared housing to veterans, homeless, seniors and the disabled in Orleans, Jefferson and Saint Charles Parishes.") + moreWebPhoneAnnounce("More information about their services, or about how you can support their work, is", "veterans housing outreach dot webs dot com", "504/340-3429"),
    orgName = Some("Veterans Housing Outreach"),
    start = "2021-06-22",
    alert = "2022-05-15",
    sourceContacts = "Lisa Carey <lisacareyrealty@yahoo.com>",
    sourceNote = "added Feb 2014"
  )

  Spot(
    "RestaurantOpportunitiesCenter",
    Services,
    str("The Restaurant Opportunities Center supports and trains New Orleans restaurant workers. They provide training in fine dining service, bartending, cooking, and English, as well as workers' rights.") + moreWebPhone("R O C united dot O R G", "504/267-4694"),
    orgName = Some("Restaurant Opportunities Center"),
    start = "2021-06-22",
    alert = "2022-05-15",
    sourceNote = "live June 2013"
  )

  Spot(
    "HomelessVetsHotline",
    Services,
    str("The US Vetarans' Administration now has a toll-free hotline to help vets who are facing homelessness. Callers will receive help with issues related to health care,  housing, employment, and education. Calls to 877/4AID-VET are confidential, and veterans are urged to use it to seek help. That number again is 877/4-A-I-D-V-E-T."),
    orgName = Some("US Vetarans' Administration"),
    start = "2021-06-22",
    alert = "2022-06-15",
    sourceContacts = Seq("Stacy Vasquez 202-461-1664 (stacy.vasquez@va.gov)", "Michael Taylor 202-461-1677 (michael.taylor3@va.gov)"),
    sourceNote = "asked June 2013"
  )

  Spot(
    "NobaSeniorDanceFitness",
    Services,
    str("The New Orleans Ballet Association offers weekly dance fitness classes for older adults ages 55 and up. Programming includes stretching, dance and movement, and healthy eating and lifestyle education.") + moreWeb("N O B A dance dot com slash senior dance fitness dot C F M"),
    orgName = Some("Ballet Association"),
    start = "2021-06-22",
    end = Some("2021-07-12"),
    alert = "2022-06-15",
    sourceContacts = "Sarah Chambless Federer <sarahc@gambelpr.com>",
    sourceNote = "add Nov 2014"
  )

  Spot(
    "CornerstoneBuildersBusProject",
    Services,
    str("The Cornerstone Builders Bus Project provides a monthly bus service to bring New Orleans residents to see loved ones in Louisiana prisons.") + moreWebAnnounce("More information, including how you can support this service, is", "nola to angola dot O R G"),
    orgName = Some("Cornerstone Builders Bus Project"),
    start = "2021-06-22",
    alert = "2022-06-15",
    sourceContacts = "Irene Rible <irene.rible@gmail.com>",
    sourceNote = "added September 2014"
  )

  Spot(
    "NolaRecycle",
    Services,
    str("The New Orleans Department of Sanitation offers a monthly recycling drop-off at 2829 Elysian Fields Avenue on the second Saturday of every month. They accept paper, plastics, metal, Mardi Gras beads, old televisions and computers, batteries, light bulbs and tires.") + moreWeb("recycle dot nola dot gov"),
    orgName = Some("New Orleans Department of Sanitation"),
    start = "2021-06-22",
    alert = "2022-06-15",
    sourceURL = "http://recycle.nola.gov/",
    sourceNote = "added March 2014"
  )

  Spot(
    "FHFboth",
    Services,
    str("Families Helping Families provides information, training and support for families of individuals with disabilities.  Two chapters serve our area: Families Helping Families of Southest Louisiana serves Orleans, Plaquemines and Saint Bernard Parishes.  Their phone number is 504/943-0343, and their website is") + online("F H F S E L A dot O R G.") + str("There is also a Jefferson Parish chapter; their number is 504/888-9111, and their website is") + online("F H F jefferson dot O R G."),
    orgName = Some("Families Helping Families"),
    start = "2021-06-22",
    alert = "2022-06-15",
    sourceContacts = "Madison Lagrone <mlagrone@fhfjefferson.org>",
    sourceNote = "updated November 2014"
  )

  Spot(
    "FamilyServiceGNO",
    Services,
    str("Family Service of Greater New Orleans works to strengthen emotional health and foster self-sufficiency of low-income families and individuals in New Orleans. They provide individual, family and group mental health counseling and services for anxiety, addiction, child abuse, depression, family violence, parent-child difficulties, homelessness and trauma.") + moreWeb("F S G N O dot O R G"),
    orgName = Some("Family Service of Greater New Orleans"),
    start = "2021-06-22",
    alert = "2022-06-15",
    sourceContacts = "pr@fsgno.org",
    sourceNote = "added March 2013"
  )

  Spot(
    "AssuranceTomorrowsLeaders",
    Services,
    str("Assurance for Tomorrow's Leaders provides services to youth aged 5--18 in Orleans, Jefferson and Saint Tammany parishes, including mental health, mentoring and enrichment services.") + morePhone("504/301-3692"),
    orgName = Some("Assurance for Tomorrow's Leaders"),
    start = "2021-06-22",
    alert = "2022-06-15",
    sourceContacts = "Kim Byas-Dilosa <gkannie95@yahoo.com>",
    sourceNote = "added October 2015"
  )

  Spot(
    "FairActionHousingCenter",
    Services,
    str("The Fair Housing Action Center helps victims of mortgage scams, discrimination in home loans, and other forms of foul play in lending.") + moreWebPhone("G N O fair housing dot O R G", "504/596-2100"),
    orgName = Some("Fair Housing Action Center"),
    start = "2021-06-22",
    alert = "2022-07-15",
    sourceContacts = "Stephanie Sheeley <ssheeley@gnofairhousing.org>",
    sourceNote = "added Dec. 2012"
  )

  Spot(
    "MortgageModification",
    Services,
    str("You may have been a victim of a mortgage modification or foreclosure scam if you were asked to pay a fee for your modification, sign over the title to your property, redirect mortgage payments, stop making payments, sign documents with blank spaces, or stop talking to your lender.  More information about how you can protect your home from mortgage scammers is available from the Fair Housing Action Center at 877/445-2100."),
    orgName = Some("Fair Housing Action Center"),
    start = "2021-06-22",
    alert = "2022-07-15",
    sourceContacts = "Amber Tucker <atucker@gnofairhousing.org>",
    sourceNote = "added Jan. 2016"
  )

  Spot(
    "MakeMusicNolaB",
    Services,
    str("Make Music NOLA provides music education to under-served students in New Orleans.  Started in 2012, Make Music NOLA educates hundreds of students in their string and general music programs in nine locations across the city.") + moreWeb("make music nola dot O R G"),
    orgName = Some("Make Music NOLA"),
    start = "2021-06-22",
    alert = "2022-07-15",
    sourceContacts = Seq(
      "Sam Rueckert <sam@makemusicnola.org>",
      "Charley Cooper/Laura Patterson <info@makemusicnola.org>",
      "laura@makemusicnola.org"),
    sourceNote = "added Jan. 2016"
  )

  Spot(
    "Lighthouse",
    Services,
    str("The Lighthouse for the Blind serves the blind and visually impaired by providing job training, competitive employment, and services.  Their visual aids store is open weekdays from eight") + am + str("to four-thirty") + (pm > comma) + str("and is located at 123 State Street.") + moreWebPhone("lighthouse louisiana dot O R G", "504/899-4501"),
    orgName = Some("Lighthouse"),
    start = "2021-06-22",
    alert = "2022-07-15",
    sourceNote = "asked June 2013"
  )

  Spot(
    "FairHousingFifteen",
    Services,
    str("The federal Fair Housing Act prohibits harassment based on race, color, national origin, religion, sex, disability, or family size. Harassment includes unwelcome sexual advances or verbal, physical and written threats. If you've experienced harassment from a landlord or property employee, you can get help at Fair Housing Action Center.") + morePhoneEmail("877/445-2100", "info at G N O fair housing dot O R G"),
    orgName = Some("Fair Housing"),
    start = "2021-06-22",
    alert = "2022-07-15",
    sourceContacts = "info@gnofairhousing.org",
    sourceNote = "received October 2015"
  )

  Spot(
    "WeHeartVeterans",
    Services,
    str("We Heart Veterans provides home care for veterans over the age of 65.  Information about qualifying for their services is available by phone at 844/816-2650."),
    orgName = Some("We Heart Veterans"),
    start = "2021-06-22",
    alert = "2022-07-15",
    sourceContacts = "Stacy Harter <sharter@wehrtvets.org>",
    sourceNote = "added January 2015"
  )

  Spot(
    "KidCameraTwo",
    Services,
    str("The New Orleans Kid Camera Project provides local children with the equipment to explore their environment and express themselves.") + moreWebAnnounce("More information, including a photograph display and upcoming events, is", "kid camera project dot O R G"),
    orgName = Some("Kid Camera"),
    start = "2021-06-22",
    alert = "2022-04-30",
    previousAlerts = Seq("2021-11-04", "2021-11-13"),
    copresent = "the New Orleans Kid Camera Project",
    sourceContacts = "info@kidcameraproject.org",
    sourceURL = "http://www.kidcameraproject.org/Contacts.html",
    sourceNote = "So email reply but still online November 2021."
  )

  Spot(
    "MetroCrisisResponse",
    Services,
    str("If you or someone you know is in crisis, Metropolitan Human Services Crisis Response can help.  A crisis could mean: a danger of hurting yourself or others; an alcohol or drug abuse crisis; or feeling overwhelmed or unstable. The Metropolitan Human Services District offers crisis services 24/7 for anyone calling from Orleans, Plaquemines and Saint Bernard Parishes. Their number is 504/568-3130."),
    orgName = Some("Metropolitan Human Services"),
    start = "2021-06-22",
    alert = "2021-11-01",
    previousAlerts = Seq("2021-11-04", "2021-11-13", "2022-02-20"),
    sourceContacts = "Brenda.Edgerton-Webster@la.gov",
    sourceNote = "added Feb. 12"
  )

  Spot(
    "HabForHumanOrientation",
    Services,
    str("The New Orleans Area Habitat for Humanity holds open house events for prospective homeowners on the first Saturday of each month at") +
      spellout("1911") +
      str("Montegut.") +
      moreWeb("habitat open house dot com"),
    orgName = Some("Habitat"),
    start = "2021-06-22",
    alert = "2021-11-01",
    end = Some("2021-11-21"),
    previousAlerts = Seq("2021-11-04", "2021-11-13"),
    sourceContacts = "communications@habitat-nola.org",
    sourceNote = "live June 2013"
  )

  Spot(
    "StarGeneral",
    Services,
    str("STAR, the Sexual Trauma Awareness and Response team, supports survivors of sexual trauma.  They operate a 24-hour hotline for anyone needing support; their number is") +
      phone("855/435-STAR") > period +
      str("STAR is also seeking volunteers to answer their hotline and accompany survivors to the hospital.") +
      moreWebAnnounce("More information about both their services and how you can help is", "star dot N G O"),
    orgName = Some("STAR"),
    start = "2021-06-22",
    alert = "2023-05-01",
    previousAlerts = Seq("2021-11-04", "2021-11-13"),
    sourceURL = "https://star.ngo/",
    sourceNote = "Checked web site Nov. 2021.  Old email contacts bounce: Margaret Reynolds <margaret.reynolds@star.ngo>, Michaela Lovejoy <michaela.lovejoy@star.ngo>"
  )

  Spot(
    "FaithHealthAlliance",
    Services,
    str("New Orleans Faith Health Alliance is a non-profit health center located in Mid-City providing primary care services to the uninsured. Services are provided on a sliding fee scale.") + morePhone("504/486-8585"),
    orgName = Some("Faith Health Alliance"),
    start = "2021-06-22",
    alert = "2021-11-01",
    previousAlerts = Seq("2021-11-04"),
    end = Some("2021-11-14"),
    sourceContacts = "information@NOFHA.org",
    sourceNote = "asked June 2013"
  )

  Spot(
    "EnvironmentProtectionAgencies",
    Services,
    str("The Louisiana Department of Environmental Quality offers a reporting line for questions and concerns: 888/763-5424. The federal EPA has a hotline for hazardous waste pickup: 800/401-1327."),
    orgName = Some("Department of Environmental Quality"),
    start = "2021-06-22",
    end = Some("2021-11-07"),
    alert = "2021-11-01",
    sourceNote = "(no email address)"
  )

  Spot(
    "EnvironmentProtectionAgencies2",
    Services,
    str("The Louisiana Department of Environmental Quality offers a reporting line for questions and concerns: 888/763-5424. The federal EPA has several hotlines for different kinds of waste and emergencies.  The federal hotline for emergency events, spills and releases is 800/424-8802.  Other federal hotlines can be found online at") + online("E P A dot gov slash about E P A slash E P A hyphen hotlines."),
    orgName = Some("Department of Environmental Quality"),
    start = "2021-11-15",
    alert = "2022-11-01",
    sourceURL = "https://www.epa.gov/aboutepa/epa-hotlines",
    sourceNote = "https://keeplouisianabeautiful.org/wp-content/uploads/2015/09/FLYER.pdf"
  )

  Spot(
    "ScriptsAssist",
    Services,
    str("If you need help paying for prescription medicines, or have recently lost your insurance coverage, you may qualify for prescription drug support programs.  More information, is available by calling the Louisiana Partnership for Prescription Assistance, 888/477-2669."),
    orgName = Some("Partnership for Prescription Assistance"),
    start = "2021-06-22",
    alert = "2021-11-05",
    end = Some("2021-11-13"),
    sourceNote = "Seems abandoned Nov. 2021.   Previous note: (old spot, phone number only) asked June 2013"
  )

  Spot(
    "LATANgeneral",
    Services,
    str("The Louisiana Assistive Technology Access Network provides information about assistive devices, services and funding to support  normal activities like bathing, walking, reading or watching TV.") + moreWebPhone("L A T A N dot O R G", "800/270-6185"),
    orgName = Some("Assistive Technology Access Network"),
    start = "2021-06-22",
    alert = "2021-11-05",
    previousAlerts = Seq("2021-11-13", "2022-02-21"),
    end = Some("2022-06-09"),
    // BOUNCES sourceContacts = "Maria Yiannopoulos <mariay@latan.org>",
    sourceNote = "Used their web form, Feb. 2022"
  )

  Spot(
    "LATANgeneralTwo",
    Services,
    str("The Louisiana Assistive Technology Access Network provides information about assistive devices, services and funding to support  normal activities like bathing, walking, reading or watching TV. LATAN has alternative financing options to make obtaining devices more affordable.") + moreWebPhoneEmail("L A T A N dot O R G", "800/270-6185", "info at L A T A N dot O R G"),
    orgName = Some("Assistive Technology Access Network"),
    start = "2022-06-10",
    alert = "2023-12-05",
    sourceContacts = "Tiffany Johnlouis <tjohnlouis@latan.org>"
  )

  Spot(
    "OdysseyReentry",
    Services,
    str("The Community Prisoner Re-entry program assists nonviolent, non-sex offenders with transition to the community after release from prison. This service is provided by Odyssey House Louisiana.") + morePhone("504/821-9211"),
    orgName = Some("Odyssey House"),
    start = "2021-06-22",
    alert = "2021-11-05",
    end = Some("2021-11-21"),
    previousAlerts = Seq("2021-11-13"),
    copresent = "Odyssey House Louisiana",
    sourceContacts = "atucker@ohlinc.org",
    sourceNote = "confirmed August 2009"
  )

  Spot(
    "ZeusRescuePetting",
    Services,
    str("Zeus's Rescues Uptown offers daily petting and cuddling of companion animals to reduce stress.  They are located at 2520 Napoleon Avenue, and are open most days.") +
      moreWebAnnounce("More information and their open hours are", "Z E U S rescues dot O R G"),
    orgName = Some("Zeus's"),
    start = "2021-06-22",
    alert = "2021-11-05",
    previousAlerts = Seq("2021-11-13", "2021-11-21", "2022-02-20"),
    sourceContacts = "zeusrescues@gmail.com"
  )

  Spot(
    "ZeusRescueGeneral",
    Services,
    str("Zeus's Rescues Uptown works to eradicate pet homelessness and euthanasia in the New Orleans metro area.") + moreWebAnnounce("More information and a link for donations are", "Z E U S rescues dot O R G"),
    orgName = Some("Zeus's"),
    start = "2021-06-22",
    alert = "2022-07-15"
  )

  Spot(
    "FreeLaMuseums",
    Services,
    str("Louisiana residents can enjoy several of New Orleans' museum admission-free every week.  Visitors need only their Louisiana ID every Wednesday at the Botanical Garden and Museum of Art, every Thursday at the Ogden Museum of Southern Art, and every Sunday at the Contemporary Arts Center.") + moreWeb("the H E L I S foundation dot O R G"),
    start = "2021-06-22",
    alert = "2022-11-05",
    previousAlerts = Seq("2021-11-13"),
    sourceContacts = "Ally Hodapp <ahodapp@bondmoroch.com>",
    sourceNote = "No email reply, but still going Nov 2021."
  )

  Spot(
    "FamilyCoachingProgramTwentyTwo",
    Services,
    str("The Louisiana Department of Health's Family Support and Coaching Program helps new and expecting parents learn all about caring for a new baby. Through this program, a qualified nurse or parent educator supports caregivers in the Greater New Orleans area as they navigate pregnancy, infant care, breastfeeding, and more. As the child grows, the program also offers tips on tracking healthy development, understanding changing behaviors, and preparing to start school.")
      + morePhone("800/251-BABY (2229)"),
    orgName = Some("Louisiana Department of Health"),
    start = "2022-06-10",
    alert = "2023-11-12",
    sourceContacts = Seq(
      "Partners for Healthy Babies <phblouisiana@gmail.com>",
      "PHBlouisiana@gmail.com", "Andrea Thames <Andrea.Thames@la.gov>")
  )

  Spot(
    "FamilyCoachingProgram",
    Services,
    str("The Family Coaching and Support Program of the Louisiana Department of Health helps new and expecting parents learn the many things to know about being a parent.  Under the  program, a nurse or parent educator will come to you and help with things like having a healthy pregnancy, caring for your newborn, breastfeeding, and helping you be the best parent you can be.  The program is tailored to each family's needs, and is a service of the state of Louisiana.") + moreWebPhoneAnnounce("More information about the program and whether you are eligible is", "partners for healthy babies dot O R G", "504/568-5926"),
    orgName = Some("Family Coaching"),
    start = "2021-06-22",
    alert = "2023-11-12",
    end = Some("2022-06-09"),
    previousAlerts = Seq("2021-11-13"),
    sourceContacts = Seq(
      "PHBlouisiana@gmail.com", "Andrea Thames <Andrea.Thames@la.gov>"),
    sourceNote = "No response from last email, but web site still going."
  )

  Spot(
    "BlindRTA",
    Services,
    str("The Regional Transit Authority (RTA) has launched a new Assistance Card Program for visiually impaired bus and streetcar passengers.  The program improves communication between operators and passengers by using color-coded cards to give information about the accommodations needed for the passenger.  The cards can be obtained at Lighthouse Louisiana's New Orleans location, 123 State Street, or at RTA Headquarters, 2817 Canal Street." + morePhoneAnnounce("The assistance cards, and more information about the program, are", "504/899-4501 extension 245")) + str("Brailed cards are available upon request."),
    orgName = Some("RTA"),
    start = "2021-06-22",
    alert = "2021-11-12",
    previousAlerts = Seq("2021-11-13", "2021-11-21"),
    sourceContacts = "Tiffany Pounds <tpounds@lighthouselouisiana.org>"
  )

  Spot(
    "NolaAbortionFund2",
    Services,
    (str("The New Orleans Abortion Fund is a nonprofit organization working across Louisiana to ensure that all people have access to quality medical care, regardless of their economic situation.  The Fund partners with local medical providers in Baton Rouge and New Orleans to provide financial assistance to patients seeking abortions who are unable to fully afford the cost.")
      + moreWeb("new orleans abortion fund dot O R G")
      + str("Their helpline for people who need assistance is")
      + phone("844/44-ABORT") > period
      + str("Callers should leave a voicemail with name and phone number; someone will return your call within twenty-four hours.")),
    orgName = Some("Abortion Fund"),
    start = "2021-11-22",
    alert = "2023-02-12",
    sourceContacts = Seq(
      "Chasity Matthews <chasity@neworleansabortionfund.org>",
      "info@neworleansabortionfund.org"),
    sourceNote = "Previous contact Steffani Bangel <steffani@neworleansabortionfund.org> bouncing."
  )

  Spot(
    "NolaAbortionFund",
    Services,
    (str("The New Orleans Abortion Fund is a nonprofit organization working across Louisiana to ensure that all people have access to quality medical care, regardless of their economic situation.  The Fund partners with local medical providers in Baton Rouge and New Orleans to provide financial assistance to patients seeking abortions who are unable to fully afford the cost.")
      + moreWeb("new orleans abortion fund dot O R G")
      + str("Their helpline for people who need assistance is 504/363-1112. Callers should leave a voicemail with name and phone number; someone will return your call within twenty-four hours.")),
    orgName = Some("Abortion Fund"),
    start = "2021-06-22",
    alert = "2021-11-12",
    end = Some("2021-11-21"),
    sourceContacts = "info@neworleansabortionfund.org",
    sourceNote = "Previous contact Steffani Bangel <steffani@neworleansabortionfund.org> bouncing."
  )

  Spot(
    "GoGreenWaterOff",
    Eco,
    str("You can save up to five gallons of water every day just by turning off the tap while brushing your teeth.  It adds up --- that's thirty five gallons a week saved during a simple everyday chore.  More tips on living green are available online at") + online("go green NOLA dot O R G slash go green tips."),
    orgName = Some("GoGreen"),
    start = "2021-06-22",
    alert = "2022-11-12",
    sourceContacts = "gogreennola@gmail.com"
  )

  Spot(
    "BuriedUtilities",
    Eco,
    str("When planning to dig holes and trenches, you can call 811 to have underground utility lines and pipelines marked.  Calling 811 before digging helps prevent disruption in vital services --- and it's the law in Louisiana.") + moreWeb("call 8 1 1 dot com"),
    start = "2021-06-22",
    alert = "2022-07-15"
  )

  Spot(
    "CompactFlorescentsGeneral",
    Eco,
    str("One of the easiest ways to lower your energy bill and fight global warming is to change the lighting in your home to compact fluorescents.  Compact fluorescents use seventy-five percent less energy than incandescent bulbs and last up to ten times longer.  If you change five of the most-used lights in your home you can save up to sixty dollars on your energy bill each year.  Each compact fluorescent prevents 450 pounds of greenhouse gas emissions over its lifetime.") + moreWebAnnounce("More energy-saving information is", "green dot tulane dot edu"),
    start = "2021-06-22",
    alert = "2021-11-19",
    end = Some("2021-11-21"),
    copresent = "The Tulane University Office of Environmental Affairs",
    orgName = Some("Environmental Affairs"),
  )

  Spot(
    "GoGreenShopLocal",
    Eco,
    str("By choosing local meat, seafood and produce, you support our economy, your body and the environment.  Community farmers' and seafood markets offer fresh food from our area, and our local summer produce can be canned, frozen or dried to last year-round.") + moreWebAnnounce("More tips on eating locally are", "go green NOLA dot O R G slash farmers markets"),
    start = "2021-06-22",
    alert = "2022-11-19",
    sourceContacts = "gogreennola@gmail.com",
    sourceNote = "Web site up Nov. 2021"
  )

  Spot(
    "NewMarreroTrashFacility",
    Eco,
    str("The new Marrero Trash Drop-Off Facility is located at 6440 Lapalco Boulevard.  They offer recycling of scrap metal, appliance, tires, batteries, oil, gasoline and antifreeze.  They also have disposal containers for waste tires, waste oil, gasoline, antifreeze, and automobile batteries.") + morePhone("504/731-4612"),
    orgName = Some("Jefferson Parish"),
    start = "2021-06-22",
    alert = "2021-11-19",
    previousAlerts = Seq("2021-11-21"),
    sourceContacts = "RCollins@jeffparish.net"
  )

  Spot(
    "JeffParishWaste",
    Eco,
    str("Oil, paint and antifreeze should never be poured down drains or put in household trash.  Jefferson Parish residents can drop off oil, paint and antifreeze for proper disposal.  Paint may be accepted at the Green Project; for more information, call 504/945-0240.  Oil and antifreeze can be brought to the David Drive or Lapalco Boulevard trash drop-off sites.  More information is available from the Jefferson Parish Department of Environmental Affairs at 504/731-4612."),
    orgName = Some("Jefferson Parish"),
    start = "2021-06-22",
    alert = "2024-11-19",
    previousAlerts = Seq("2021-11-21"),
    sourceContacts = Seq("info@thegreenproject.org", "RCollins@jeffparish.net")
  )

  Spot(
    "GulfRestNetFive",
    Eco,
    str("We can restore Louisiana's coast, but it will take hard work and tough choices. Coastal lines of defense are natural and man-made landscape features necessary to protect coastal Louisiana.") + moreWeb("lines of defense dot O R G"),
    start = "2021-06-22",
    alert = "2021-11-19",
    previousAlerts = Seq("2021-11-21"),
    // BOUNCES sourceContacts = "Robert Smith <robert@healthygulf.org>",
    sourceNote = "(updated Feb 2012) alternate with GulfRestNetFour --- Feb 2022 used their web form to reconfirm"
  )

  Spot(
    "GreenProjectElectronicsB",
    Eco,
    str("Computers, monitors, mobile phones and other electronic devices release toxic substances like lead, cadmium, and mercury into our landfills and water when discarded improperly.  You can properly dispose of electronics at The Green Project,") +
      spellout("2831") +
      phonetic("Marais", "mah-RAY", "ɪməˈɹeɪ") +
      str("Street.") +
      moreWeb("the green project dot O R G"),
    orgName = Some("Green Project electronic waste"),
    start = "2021-06-22",
    end = Some("2022-03-05"),
    alert = "2021-11-26",
    previousAlerts = Seq("2022-02-21"),
    sourceContacts = "info@thegreenproject.org"
  )

  Spot(
    "GoGreenCompost",
    Eco,
    str("Most organic trash can be used to feed our gardens.  More information about how to set up composting in your garden, and what kitchen waste can be composted is available from") +
      online("go green nola dot O R G slash gardening one zero one."),
    start = "2021-06-22",
    alert = "2021-11-26",
    orgName = Some("Go Green Composting"),
    previousAlerts = Seq("2022-02-20"),
    sourceContacts = "gogreennola@gmail.com"
  )

  Spot(
    "GreenProjectConstructionWasteB",
    Eco,
    str("The Green Project recycles building materials, which are then available for rebuilding and renovation of homes, reducing the waste going to landfills.  More information is available from The Green Project at") +
      spellout("2831") +
      phonetic("Marais", "mah-RAY", "məˈɹeɪ") +
      str("Street, or online at") +
      online("the green project dot O R G."),
    orgName = Some("Green Project Construction Waste"),
    start = "2021-06-22",
    alert = "2024-11-19",
    previousAlerts = Seq("2022-02-20"),
    sourceContacts = "info@thegreenproject.org",
    sourceNote = "(added Dec. 2008)"
  )

  Spot(
    "AlcAnon",
    Health,
    str("Alcoholics Anonymous offers support when drinking is no longer fun, and separates you from other people.  Their meetings offer support from others who are making their way to sobriety.") +
      moreWebPhoneAnnounce("More information, and a schedule of meetings for the New Orleans area, are",
        "A A new orleans dot O R G", "504/838-3399"),
    orgName = Some("Alcoholics Anonymous"),
    start = "2021-06-22",
    alert = "2021-11-26",
    previousAlerts = Seq("2022-02-20"),
    sourceContacts = "aa.nola.pi.cpc@gmail.com"
  )

  Spot(
    "DeprBipolarAllianceC",
    Health,
    str("The Depression and Bipolar Support Alliance is a peer support group for people with bipolar disorder or depression, their families, and their friends.  The alliance meets the first and third Tuesdays of every month from seven to eight") +
      pm +
      str("online, and periodically outside when weather permits.") +
      moreWebPhone("www dot D B S A New Orleans dot O R G", "504/286-1916"),
    orgName = Some("Depression and Bipolar Support Alliance"),
    start = "2021-06-22",
    alert = "2021-11-26",
    sourceURL = "https://www.dbsaneworleans.org/"
  )

  Spot(
    "PlannedParenthood",
    Health,
    str("Planned Parenthood affiliate health centers provide culturally competent, high quality, affordable health care to millions of women, men, and teens every year.  The New Orleans Planned Parenthood Center on Magazine Street is currently open with limited clinical services, including access to emergency contraception.  Emergency contraception can prevent pregnancy if taken up to one hundred twenty hours following unprotected sex.  Their numbers for medical questions, or for scheduling an appointment with the Planned Parenthood health center nearest you, are") +
      phone("800/230-PLAN") +
      str("and") +
      phone("1-800-230-7526") > period,
    orgName = Some("Planned Parenthood"),
    start = "2021-06-22",
    alert = "2021-11-26",
    end = Some("2022-09-17"),
    copresent = "Planned Parenthood",
    sourceNote = "(no contact info)",
    boost = 0.6
  )

  Spot(
    "AccidentsHappen", Health,
    str("Accidents happen.  If you have had unprotected sex in the past five days, you do not have to wait for a period that may never come. Emergency contraception can prevent pregnancy for up to one hundred twenty hours after sex.  It is available at your local Planned Parenthood office.  Their phone number is") +
      phone("800/230-PLAN") > period,
    orgName = Some("Planned Parenthood"),
    start = "2021-06-22",
    alert = "2021-11-26",
    end = Some("2022-09-17"),
    copresent = "Planned Parenthood",
    sourceNote = "[tweets through Aug. '10]",
    boost = 0.6
  )

//    ## live meetings, pull COVID # 'NoAidsTesting',
//    # 'TargetsAdultBullying',  ## live meetings, pull COVID

  Spot(
    "BloodCenterOne",
    Health,
    str("Each and every day, 300 to 350 people are needed to give blood in Southeast Louisiana.  Sixty percent of the population is eligible to donate blood, but only five percent actually do.  The Blood Center has four convenient donor centers in the New Orleans area, or our mobile team can bring the drive to you.") +
      moreWebPhoneAnnounce(
        "Center hours and addresses, and information about booking a blood drive, are",
        "www dot the blood center dot O R G", "800/86-BLOOD"),
    orgName = Some("The Blood Center"),
    start = "2021-06-22",
    end = Some("2022-03-05"),
    alert = "2021-12-04",
    previousAlerts = Seq("2022-02-20"),
    copresent = "The Blood Center",
    sourceContacts = "info@thebloodcenter.org"
  )

  Spot(
    "BloodCenterTwo",
    Health,
    str("The Blood Center has locations around the New Orleans area for blood donors to respond to hospital, patient and emergency needs.  Or, a mobile team can bring the blood drive to school, business, civic or religious organizations.") +
      moreWebPhoneAnnounce(
        "Center hours and addresses, and information about booking a blood drive, are",
        "the blood center dot O R G", "800/86-BLOOD"),
    orgName = Some("The Blood Center"),
    start = "2022-03-05",
    alert = "2023-12-04",
    previousAlerts = Seq(),
    copresent = "The Blood Center",
    sourceContacts = Seq(
      "info@thebloodcenter.org", "Paul Adams <padams@thebloodcenter.org>")
  )

  Spot(
    "StutteringFoundation",
    Health,
    str("Anything that hurts your ability to communicate can limit your life. If you, your child, or someone you know has a stuttering problem, the Stuttering Foundation can help.") +
      moreWebPhone("stuttering help dot O R G", "800/992-9392"),
    orgName = Some("Stuttering Foundation"),
    start = "2021-06-22",
    alert = "2025-12-04",
    previousAlerts = Seq("2022-02-20"),
    sourceContacts = "Jane Fraser <jfraser@stutteringhelp.org>"
  )

  Spot(
    "RapeCrisis",
    Health,
    str("The Greater New Orleans Center for Women and Children operates programs to assist victims of domestic violence and sexual assault. Services include a 24 hour confidential crisis line, shelter for victims of domestic violence, limited legal assistance (including referrals), and counseling and advocacy services.  All services are confidential and are provided at no cost by specially trained staff members.  Help is provided both for victims, and for people who want to stop committing violent acts.  Assistance is available by calling") +
      phone("504/837-5400") > period,
    orgName = Some("Center for Women and Children"),
    start = "2021-06-22",
    alert = "2021-12-04",
    copresent = "the Greater New Orleans Center for Women and Children",
    sourceNote = "[tweets through June 10]",
    groupGainMultiplier = 1.2,
    boost = 0.65
  )

  Spot(
    "InternetPrescriptions",
    Health,
    str("Increasing numbers of fraudulent websites are selling counterfeit medicines.  You can avoid danger when purchasing drugs online by following a few simple safety guidelines. Legitimate e-pharmacies will not allow you to purchase prescription medication without a valid prescription. Legitimate e-pharmacies should also have an easy way for you to contact a real pharmacist for a medical consultation. Risks can be further reduced by avoiding sites that do not display a physical") +
      spellout("US") +
      str("street address and a toll-free phone number, and avoiding websites that sell only") +
      doublequoted("lifestyle") +
      phone("medications used to treat conditions such as obesity and impotence.  The National Association of Boards of Pharmacy has created a web site to identify online pharmacies that are legitimate and appropriately licensed at") + online("V I P P S dot info."),
    start = "2021-06-22",
    alert = "2021-12-04",
    end = Some("2022-05-31"),
    previousAlerts = Seq("2022-02-20"),
    copresent = "the Partnership for Safe Medicines",
    orgName = Some("Partnership for Safe Medicines"),
    sourceContacts = "Chad Wilkisnon <chad@keybridge.biz>"
  )

  Spot(
    "LouisianaCoalitionDomesticViolence",
    Health,
    str("The Louisiana Domestic Violence Hotline is") +
      phone("888/411-1333") > period +
      str("The hotline is free, confidential and available 24-hours-a-day. They can help your family, or help you to help others. Their number again is") +
      phone("888/411-1333") > period,
    orgName = Some("Domestic Violence Hotline"),
    start = "2021-06-22",
    alert = "2021-12-04",
    previousAlerts = Seq("2022-02-20"),
    // BOUNCES sourceContacts = "Alex Juan <alex.juan@lcadv.org>",
    sourceContacts = "info@lcadv.org",
    groupGainMultiplier = 1.2
  )

  Spot(
    "OdysseySubstance2",
    Health,
    str("Odyssey House Louisiana provides substance use disorder treatment, including detox, short-term and long-term treatment. The Odyssey House Louisiana Community Health Center provides primary care and behavioral health care services.  COVID-19 vaccines are also available.") +
      moreWeb("O H L I N C dot O R G"),
    orgName = Some("Odyssey House"),
    start = "2021-06-22",
    alert = "2023-04-04",
    copresent = "Odyssey House Louisiana",
    sourceNote = "confirmed November 2021",
    sourceContacts = Seq("Ann Tucker <atucker@ohlinc.org>")
  )

  Spot(
    "OdysseySubstance",
    Health,
    str("Odyssey House Louisiana is a nonprofit behavioral healthcare facility with an emphasis on addiction treatment, providing detoxification, residential treatment and outpatient treatment. Other services include a community medical clinic, community prisoner reentry and housing services for homeless populations.") +
      moreWebPhone("504/821-9211", "O H L I N C dot O R G"),
    orgName = Some("Odyssey House"),
    start = "2021-06-22",
    alert = "2021-12-04",
    end = Some("2021-11-21"),
    copresent = "Odyssey House Louisiana",
    sourceNote = "confirmed August 2009 - atucker@ohlinc.org",
    sourceContacts = "atucker@ohlinc.org"
  )

  Spot(
    "OrganDonationLegacy",
    Health,
    str("Today, there are nearly 100,000 people on the national organ transplant waiting list, and more than 1,700 of them live here in Louisiana.") +
      moreWebAnnounce("More information about organ and tissue donation is",
        "organ awareness dot O R G"),
    start = "2021-06-22",
    alert = "2021-12-11",
    previousAlerts = Seq("2022-02-20"),
    copresent = "Legacy Donor Foundation",
    orgName = Some("Legacy Donor Foundation"),
    sourceContacts = "info@organawareness.org"
  )

  Spot(
    "TulaneWomenHealthServices",
    Health,
    str("The all-female staff of the Student Health Center's Women's Health Clinic provides Tulane students with STD counseling and testing, PAP exams, colposcopy, breast exams, contraceptive counseling, smoking cessation, urinary symptoms treatment, HPV vaccination, and sexual assault care.") +
      morePhone("504/865-5255"),
    orgName = Some("Women's Health Clinic"),
    start = "2021-06-22",
    alert = "2021-12-11",
    copresent = "The Tulane Women's Health Clinic"
  )

  Spot(
    "SAPHE",
    Health,
    phonetic("SAPHE", "safe", "seɪf") > comma +
      str("Tulane's Sexual Aggression Peer Hotline and Education program, provides the Tulane community with resources, support and education about sexual aggression. If you or someone you know is a victim of sexual assault, relationship violence, stalking, harassment, or exploitation, you can call") +
      phonetic("SAPHE's", "safes", "seɪfs") +
      str("student-operated, confidential hotline at") +
      phone("504/654-9543") > period +
      str("Again, the hotline's number is") +
      phone("504/654-9543") > period,
    orgName = Some("SAPHE"),
    start = "2021-06-22",
    alert = "2021-12-11",
    previousAlerts = Seq("2022-02-20"),
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
    str("Smoking is the most preventable cause of death in our society.  The American Cancer Society offers information about quitting smoking, and about local resources.") +
      moreWebPhone("cancer dot O R G", "800/ACS-2345"),
    orgName = Some("American Cancer Society"),
    start = "2021-06-22",
    alert = "2023-12-11",
    previousAlerts = Seq("2022-02-20"),
    // BOUNCES sourceContacts = "Rebecca Do <rydo@loyno.edu>"
    sourceContacts = Seq(
      "Lindsey Shirley <lindsey.shirley@cancer.org>", // From website
      "Kym McGee <kym.mcgee@cancer.org>") // Email exchange Feb. '22
  )

  Spot(
    "TesticularCancerA",
    Health,
    str("Testicular cancer is the most common cancer in men between the ages of 15 and 35.  When detected early, both sexual function and fertility can be preserved.  But if left untreated, the disease will spread, and become life-threatening.   A simple and effective self-examination is available from your doctor, or online at") +
      online("T C R C dot A C O R dot O R G."),
    start = "2021-06-22",
    alert = "2021-12-11",
    copresent = "the Testicular Cancer Resource Center",
    orgName = Some("Testicular Cancer Resource Center")
  )

  Spot(
    "MedicarePlanFinder",
    Health,
    str("Medicare Part D, the prescription drug benefit for seniors, now offers an online service for beneficiaries to find and compare prescription drug plans.  The service, called") +
      doublequoted("Plan Finder") > comma +
      str("is part of the website") +
      online("medicare dot gov.") +
      str("To use the Plan Finder, seniors will need to first make a list of their current prescriptions and dosages.  With this information, Plan Finder will return a list of appropriate insurance plans, and their costs in your area.  Plan Finder can help you navigate the hundreds of private plans competing under Medicare Part D.") +
      moreWebPhone("medicare dot gov", "1-800-MEDICARE"),
    start = "2021-06-22",
    alert = "2021-12-11",
    copresent = "the Center for Medicine in the Public Interest",
    orgName = Some("Center")
  )

  Spot(
    "StBaldricksFoundation",
    Health,
    str("The Saint Baldrick's Foundation raises money to support research into curing childhood cancer. More information about their work, making a donation, or holding a fundraiser is available on their website,") +
      online("S T baldricks dot O R G."),
    orgName = Some("Saint Baldrick's Foundation"),
    start = "2021-06-22",
    alert = "2022-07-15"
  )

  Spot(
    "Prostate",
    Health,
    str("Nearly two hundred thousand American will be diagnosed with prostate cancer this year, but it is over 90 percent curable when detected early.  Information on getting a simple and painless screening is available from the Prostate Cancer Education Council.") +
      moreWebPhone("P C A W dot com", "866/477-6788"),
    orgName = Some("Prostate Cancer Education Council"),
    start = "2021-06-22",
    alert = "2021-12-18",
    sourceURL = "https://www.pcaw.com/",
    copresent = "the Prostate Cancer Education Council"
  )

  Spot(
    "HeartDiseaseGeneral",
    Health,
    str("Heart disease is the number one killer in New Orleans, Louisiana and America. It kills more people each year than all cancers combined, and does not discriminate by age, race or gender. " +
      moreWebAnnounce("More information about what heart disease is, risk factors that may affect you or your loved ones, and how you can get involved to save lives is", "heart dot O R G")),
    start = "2021-06-22",
    alert = "2022-07-15"
  )

  Spot(
    "EyesHaveIt",
    Health,
    str("Children with uncorrected vision conditions can face all sorts of life barriers --- academically, psychologically and socially. Remember to have your child's eyes checked regularly.") +
      moreWeb("www dot eyes for school dot O R G"),
    start = "2021-06-22",
    alert = "2021-12-18",
    end = Some("2021-11-21"),
    sourceContacts = "Susan Ilijevich <susan@whitespace-creative.com>"
  )

  Spot(
    "NarcanonScriptDrugAbuse",
    Health,
    str("Prescription drug abuse is on the rise.  What's in your medicine cabinet?  More information about how to keep your kids safe is available from Narconon at") +
      phone("877/413-3073") > comma +
      str("or online at") + online("drugs no dot com."),
    orgName = Some("Narconon"),
    start = "2021-06-22",
    alert = "2021-12-18",
    copresent = "Narcanon",
    sourceNote = "(added Jan 09)"
  )

  Spot(
    "NationalSuicidePrev",
    Mental,
    str("The National Suicide Prevention crisis counseling hotline is") +
      phone("800/273-8255") > period +
      str("This number is available both for people who are having suicidal thoughts, and who are concerned about others.  That number again is") +
      phone("800/273-8255") > period,
    orgName = Some("Suicide Prevention Hotline"),
    start = "2021-06-22",
    alert = "2021-12-18",
    sourceNote = "(phone number only, last called to check 8/2006)"
  )

  Spot(
    "BehavioralHealthCrisisLine",
    Mental,
    str("If you or someone you know is in a behavioral health crisis, the Metropolitan Crisis Response Team can help.  A behavioral health crisis could mean: being in danger of hurting themselves or others; being in need of emergency housing; having an alcohol or drug crisis; or when a person with mental illness feels overwhelmed or unstable. The New Orleans Health Department runs a 24-hour hotline for Orleans, Plaquemines and Saint Bernard Parishes. Their number is") +
      phone("504/826-2675") > period +
      str("For individuals in crisis in Jefferson Parish, the number is") +
      phone("504/832-5123") > period,
    orgName = Some("Metropolitan Crisis Response Team"),
    start = "2021-06-22",
    alert = "2024-08-15",
    sourceURL = "http://new.nola.gov/health-department/behavioral-health/behavioral-health-resources/",
    groupGainMultiplier = 1.2
  )

  Spot(
    "RebuildingTogetherNewOrleans",
    Civic,
    str("Rebuilding Together New Orleans is a volunteer group rehabiliting houses across New Orleans.  These repairs strengthen neighborhoods by allowing low-income homeowners to stay in their homes as they age, inhibiting blight, and keeping invested homeowners in place.") + moreWebPhoneAnnounce("More information about volunteering with them is", "R T N O dot O R G", "504/264-1815"),
    orgName = Some("Rebuilding Together"),
    start = "2021-06-22",
    end = Some("2021-07-11"),
    previousAlerts = Seq("2022-02-20"),
    alert = "2021-12-18",
    sourceContacts = "Alex Thibadoux <athibadoux@rtno.org>"
  )

  Spot(
    "RebuildingTogetherNewOrleansJuly2021",
    Civic,
    str("Rebuilding Together New Orleans is seeking volunteers to help with the rehabilitation of houses across New Orleans. These repairs strengthen neighborhoods by allowing low-income homeowners to stay in their homes as they age, inhibiting blight from spreading and keeping invested homeowners in place.") +
      moreWebPhoneAnnounce("More information about volunteering with them is", "R T N O dot O R G", "504/264-1815"),
    orgName = Some("Rebuilding Together"),
    start = "2021-07-12",
    alert = "2022-01-05",
    sourceContacts = "Alex Thibadoux <athibadoux@rtno.org>"
  )

  Spot(
    "FriendsOfAlgiersFerryTwo",
    Civic,
    str("Friends of the Ferry works to promote the Canal Street/") >
      phonetic("Algiers", "al-JEERS") +
      str("Ferry as a convenient and inexpensive form of public transportation.") +
      moreWebPhone("friends of the ferry dot O R G", "504/363-9090"),
    orgName = Some("Friends of the Ferry"),
    start = "2021-06-22",
    alert = "2022-01-05",
    sourceNote = "(re-confirmed October 2008)"
  )

  Spot(
    "SeatBeltBuckle", Civic,
    str("New Orleanians are less likely than most to buckle up while driving.  However seat belts save lives, and are required by law whether you are in the front or back seat of a vehicle."),
    start = "2021-06-22",
    alert = "2029-07-15",
    copresent = "the New Orleans Regional Traffic Safety Coalition",
    sourceContacts = "Emilie Bahr <ebahr@norpc.org>"
  )

  Spot(
    "UrbanConservancy",
    Civic,
    str("The Urban Conservancy promotes the wise stewardship and equitable access for our city's and region's rich economic, environmental and cultural assets.  Current projects emphasize responsible land use to align our natural and built environments and a strong local economy.") +
      moreWebEmail("urban conservancy dot O R G",
        "info at urban conservancy dot O R G"),
    orgName = Some("Urban Conservancy"),
    start = "2021-06-22",
    alert = "2025-01-05",
    previousAlerts = Seq("2022-02-20"),
    sourceContacts = Seq(
      "Dana Eness <dana@urbanconservancy.org>",
      "Maryann Miller <maryann@staylocal.org>",
      "Sam Commagere <sam@urbanconservancy.org>")
  )

  Spot(
    "IrsProvideTaxReturns2",
    Civic,
    str("Taxpayers need copies or transcripts of prior years' tax returns for many reasons. You should keep copies of your tax returns, but if they cannot be located or have been destroyed during natural disasters or by fire, the") +
      spellout("IRS") +
      str("can help. You can reach the") +
      spellout("IRS") +
      str("for via the web, phone or by mail. Their website is") +
      online("I R S dot gov"),
    start = "2021-06-22",
    alert = "2026-07-15"
  )

  Spot(
    "IrsProvideTaxReturns",
    Civic,
    str("People need copies or transcripts of prior years' tax returns for many reasons. You should keep copies of your tax returns, but if they cannot be located or have been destroyed during natural disasters or by fire, the") +
      spellout("IRS") +
      str("can help. You can reach the") +
      spellout("IRS") +
      str("for via the web, phone or by mail. Their website is") + online("I R S dot gov"),
    start = "2021-06-22",
    end = Some("2021-11-21"),
    alert = "2026-07-15"
  )

  Spot(
    "LouisianaDisasterRecoveryFoundation",
    Civic,
    str("The Louisiana Disaster Recovery Foundation supports long-term and equitable recovery throughout Louisiana, and provides assistance to citizens in need through a network of Louisiana charities and nonprofit agencies.") +
      moreWebPhone("louisiana help dot O R G", "877/435-7521"),
    orgName = Some("Disaster Recovery Foundation"),
    start = "2021-06-22",
    alert = "2022-01-05",
    sourceNote = "still going Aug. 09, jkemp@louisianahelp.org"
  )

  Spot(
    "TerrytownCivic",
    Civic,
    str("The Terrytown Civic Association meets the first Wednesday of each month at seven") + pm + str("at the Golden Age Center, 604 Heritage Avenue.") + moreWebPhone("terrytown L A dot O R G", "504/914-2200"),
    start = "2021-06-22",
    end = Some("2021-06-22"),
    alert = "2021-06-15",
    previousAlerts = Seq("2021-10-25"),
    orgName = "Terrytown Civic Association",
    sourceContacts = "Stephen Leonard <president@terrytownla.org>"
  )

  Spot(
    "TulaneGalvezPheonix", Civic,
    str("The Phoenix of New Orleans is the neighborhood recovery association for the Tulane/") >
      phonetic("Gravier", "GRAH-vee-ur") +
      "neighborhood, bordered by I-10, Saint Louis, Claiborne Avenue and Broad Street.  The Phoenix provides community organization, directs recovery services, and sponsors monthly neighborhood meetings." +
      moreWebPhoneAnnounce("More information and volunteer opportunities are", "www dot P N O L A dot O R G", "504/342-4399"),
    orgName = Some("Phoenix of New Orleans"),
    start = "2021-06-22",
    alert = "2022-01-12",
    end = Some("2021-11-21"),
    copresent = "The Phoenix Group",
    sourceContacts = "Paul Ikemire <director@pnola.org>"
  )

  Spot(
    "FaubourgStJohnNeighborhoodAssocTwo",
    Civic,
    str("The") +
      phonetic("Faubourg", "FOH-borg") +
      str("Saint John Neighborhood Association serves the areas between the Fairgrounds, City Park, Orleans Avenue and North Broad Street.  They sponsor regular meetings, cleanups, walks and other neighborhood get-togethers.") +
      moreWebAnnounce("More information and volunteer opportunities are",
        "F S J N A dot O R G"),
    orgName = Some("Saint John Neighborhood Association"),
    start = "2021-06-22",
    alert = "2022-01-12",
    previousAlerts = Seq("2022-02-20"),
    copresent = "The Faubourg Saint John Neighborhood Association",
    sourceContacts = "info@fsjna.org"
  )

  Spot(
    "IRSfakers",
    Civic,
    str("The") +
      spellout("IRS") +
      str("is alerting taxpayers about email scams that direct individuals to fake websites which demand personal information like social security numbers and bank account numbers.  The") +
      spellout("IRS") +
      str("does not communicate by email, by text message, or over social media like Facebook. The") +
      spellout("IRS") +
      str("encourages people to protect their personal and financial information at all times.  Make sure you are visiting the official") +
      spellout("IRS") +
      str("website,") + online("I R S dot gov") > period +
      str("Do not fall prey to cyberthieves by visiting fake websites ending in dot-com, dot-net, or dot-org."),
    orgName = Some("IRS"),
    start = "2021-06-22",
    alert = "2022-01-12",
    copresent = "the IRS"
  )

  Spot(
    "EmployExCons",
    Civic,
    str("Employers: you can help reduce the recidivism rate in New Orleans by employing formerly incarcerated individuals.  Steady employment is an important factor in successfully reintegrating back into the community, and avoiding re-offending.  Federal and state funds may be available.") + morePhone("504/568-8738"),
    start = "2021-06-22",
    alert = "2022-01-12",
    previousAlerts = Seq("2022-02-20"),
    copresent = "the Department of Public Safety \\& Corrections, Division of Probation \\& Parole",
    orgName = Some("Public Safety \\& Corrections"),
    sourceContacts = "tmccoy@corrections.state.la.us (Thomas McCoy)"
  )

  Spot(
    "LowerNinthHomeowners",
    Civic,
    str("The Lower Ninth Ward Homeowners' Association meets monthly, on the second Saturday of the month, at one") + pm +
      str("at Holy Angels Academy,") +
      spellout("3500") +
      str("Saint Claude Avenue."),
    orgName = Some("Lower Ninth Ward Homeowners' Association"),
    start = "2021-06-22",
    end = Some("2021-06-22"),
    alert = "2022-06-15",
    copresent = "the Lower Ninth Ward Homeowners' Association",
    sourceContacts = "Linda Davis <lindadavisis@yahoo.com>"
  )

  Spot(
    "ParkwayPartnersGeneral",
    Civic,
    str("For thirty years, Parkway Partners has provided education and volunteers to enhance our neutral grounds, parks, schoolyards and community gardens.") +
      moreWebAnnounce(
        "More information about what they do, and how you can support them, is",
        "parkway partners nola dot O R G"),
    orgName = Some("Parkway Partners"),
    start = "2021-06-22",
    alert = "2022-07-15"
  )

  Spot(
    "NOLAneighDevColl",
    Civic,
    str("New Orleans Neighborhood Development Collaborative helps low- and moderate-income families find high-quality housing. They are currently offering grants to qualified families including Section 8 and public housing residents.") +
      moreWebPhone("harmony homes nola dot com", "504/302-8696"),
    orgName = Some("Neighborhood Development Collaborative"),
    start = "2021-06-22",
    alert = "2022-07-15"
  )

  Spot(
    "NolaThreeOneOne",
    Civic,
    str("NOLA 311 handles non-emergency city service requests such as City Assisted Evacuation registration, code enforcement complaints, street and sidewalk maintenance, street and traffic signs, traffic signals, street lights, abandoned vehicles and sanitation issues.  You can reach them weekdays from eight") +
      am + str("to five") + pm +
      str("by dialing") +
      spellout("311") +
      str("from any phone based in New Orleans, or") +
      phone("504/658-2299") > comma +
      str("or") +
      phone("877/286-6431") > period,
    orgName = Some("NOLA 311"),
    start = "2021-06-22",
    alert = "2022-01-12",
    sourceNote = "added Sept 2012"
  )

  Spot(
    "ArabAntiDiscTwo",
    Civic,
    str("In recent years, bias against Arab Americans has grown throughout the") +
      spellout("US") > period +
      spellout("The American Arab Anti-Discrimination Committee (or A D C) is a grassroots civil rights organization working to combat discrimination, bias and stereotyping.") +
      moreWebAnnounce(
        "More information about the New Orleans chapter of the A D C is",
        "A D C dot O R G"),
    orgName = Some("ADC"),
    start = "2021-06-22",
    alert = "2022-01-19",
    previousAlerts = Seq("2022-02-20"),
    copresent = "American Arab Anti-Discrimination Committee",
    sourceContacts = "info@nolahumanrights.org"
  )

  Spot(
    "TransparencyAccountability",
    Civic,
    str("The Project To Promote Transparency and Accountability in Government holds a series of meetings at which residents are invited to offer their input into the city's priority-setting and decision-making.") +
      morePhone("504/267-4665 or 267-4696"),
    orgName = Some("Project To Promote Transparency and Accountability in Government"),
    start = "2021-06-22",
    alert = "2022-01-19",
    end = Some("2021-11-21"),
    sourceNote = "(from the Times-Pic)"
  )

  Spot(
    "YouthEmpowermentProjectBikes",
    Civic,
    str("The Youth Empowerment Project serves at-risk New Orleans youth by providing professional training in a bike shop setting. They are seeking donations of used bicycles and parts in any condition for their training program.") +
      moreWebPhoneAnnounce(
        "More information, plus scheduling for donation pick-up or drop-off, are",
        "youth empowerment project dot O R G",
        "504/875-4301"),
    orgName = Some("Youth Empowerment Project"),
    start = "2021-06-22",
    alert = "2022-01-19",
    sourceNote = "bwhite@youthempowermentproject.org"
  )

  Spot(
    "FederalDepositoryLibrary",
    Civic,
    str("Are you interested in information on science, technology, health, laws, the workings of the presidential administration, and more?  Your local Federal Depository Library provides open access to") +
      spellout("US") +
      str("government information.  You can find the library nearest you at") +
      online("G P O dot gov slash libraries."),
    start = "2021-06-22",
    alert = "2026-07-15"
  )

  Spot(
    "CentralCityRenaissance", Civic,
    str("The Central City Renaissance Alliance meets monthly to discuss community issues and resources available to Central City.  Information about their next meeting is on their answering machine at") +
      phone("504/581-5301") > period +
      str("More information about the Alliance is available at the same number,") +
      phone("504/581-5301") > period,
    orgName = Some("Central City Renaissance Alliance"),
    start = "2021-06-22",
    alert = "2022-01-19",
    previousAlerts = Seq("2022-02-20"),
    copresent = "the Central City Renaissance Alliance",
    sourceContacts = "Dorian Hastings <d_hastings@juno.com>"
  )

  Spot(
    "MidCityCommunityMeeting", Civic,
    str("Mid-City Recovery Action Meetings take place on the first Monday of each month at six-thirty") + pm +
      str("at Grace Episcopal Church, 3700 Canal Street.  Their meetings are open to the public, and all are invited to help rebuild Mid-City. More information is available on the Mid-City Neigbhorhood Organization website,") +
      online("M C N O dot O R G."),
    orgName = Some("Mid-City Neigbhorhood Organization"),
    start = "2021-06-22",
    end = Some("2021-06-22"),
    alert = "2021-05-15",
    previousAlerts = Seq("2021-10-25"), // Not running --- do not alert
    copresent = "Mid-City Neigbhorhood Organization",
    sourceContacts = "Bart <b@rox.com>"
  )

  Spot(
    "PAWS",
    Animal,
    str("PAWS, the") +
      phonetic(str("Plaquemines"), "PLACK-minzz", "ˈplæk.məŋz") +
      str("Animal Welfare Society, cares for unwanted, abused and abandoned animals in") +
      phonetic(str("Plaquemines"), "PLACK-minzz", "ˈplæk.məŋz") +
      str("Parish, and works to find new homes for these animals.")
      + moreWeb("paws 4 life dot O R G"),
    orgName = Some("PAWS"),
    start = "2021-06-22",
    alert = "2022-01-19",
    previousAlerts = Seq("2022-02-20"),
    sourceContacts = "pawslouisiana@gmail.com"
  )

  Spot(
    "SPCAone",
    Animal,
    str("The Louisiana") +
      spellout("SPCA") +
      str("needs volunteers. Volunteers can help by responding to telephone inquiries, assisting with arriving animals, exercising and socializing animals, providing foster care and vet clinic assistance, in addition to many other roles.")
      + moreWebAnnounce("More information and a volunteer application are",
          "L A hyphen S P C A dot O R G"),
    orgName = Some("Louisiana SPCA"),
    start = "2021-06-22",
    alert = "2022-01-26",
    copresent = "the Louisiana SPCA"
  )

  Spot(
    "HumaneSocLou",
    Animal,
    str("The Humane Society of Louisiana investigates animal cruelty, rescues and fosters abused and neglected animals, and provides animal care information to the public.  They are seeking foster homes, volunteers and donations.") +
      moreWebPhone("humane L A dot O R G", "888/6-HUMANE"),
    orgName = Some("Humane Society of Louisiana"),
    start = "2021-06-22",
    alert = "2022-01-26",
    previousAlerts = Seq("2022-02-20"),
    copresent = "Humane Society of Louisiana",
    sourceContacts = "Robyn B. <marshmallowlullaby@hotmail.com>"
  )

  Spot(
    "SPCAtwoRevOhSeven",
    Animal,
    str("The Louisiana") +
      spellout("SPCA") +
      str("can help when someone loses a pet, knows of an animal needing help, or needs to report an animal control issue.  Their number is 504/368-5191.  The") +
      spellout("LSPCA") +
      str("can also assist pet owners trying locate pets lost after Katrina.  Their number again is 504/368-5191."),
    orgName = Some("Louisiana SPCA"),
    start = "2021-06-22",
    alert = "2022-01-26",
    end = Some("2021-11-21"),
    copresent = "the Louisiana SPCA"
  )

  Spot(
    "SPCAtwoRev2021",
    Animal,
    str("The Louisiana") +
      spellout("SPCA") +
      str("can help when someone loses a pet, knows of an animal needing help, or needs to report an animal control issue.  Their number is") +
      phone("504/368-5191") > period +
      str("The") +
      spellout("LSPCA") +
      str("can also assist pet owners trying locate pets lost after storms.  Their number again is") +
      phone("504/368-5191") > period,
    orgName = Some("Louisiana SPCA"),
    start = "2021-06-22",
    alert = "2022-01-26",
    copresent = "the Louisiana SPCA"
  )

  Spot(
    "MicrochippingSPCA",
    Animal,
    str("Microchipping is a quick, easy way to ensure that your dog or cat will never be without their identifying information.  The Louisiana") +
      spellout("SPCA") +
      str("partners with local businesses to offer low-cost microchipping events around the New Orleans metro area.") +
      moreWebAnnounce("The time and place of the next microchipping event is", "www dot L A hyphen S P C A dot O R G"),
    orgName = Some("LSPCA"),
    start = "2021-06-22",
    alert = "2022-01-26",
    copresent = "the Louisiana SPCA"
  )

  Spot(
    "SpaymartTwo",
    Animal,
    str("Spaymart Sanctuary is a New Orleans non-profit cat adoption and spay neuter organization.  They are seeking volunteers and prospective cat owners.") + moreWebPhone("spaymart dot O R G", "601/749-0268"),
    orgName = Some("Spaymart Sanctuary"),
    start = "2021-06-22",
    alert = "2022-01-26",
    previousAlerts = Seq("2022-02-20"),
    copresent = "Spaymart",
    sourceContacts = "Lynn Chiche <lynn@spaymart.org>"
  )

  Spot(
    "AnimalRescueNewOrleansB",
    Animal,
    str("Animal Rescue New Orleans is a nonprofit no-kill animal shelter located at") +
      spellout("271") +
      phonetic(str("Plauche"), "PLAW-shay", "ˈplɔ.ʃeɪ") +
      str("Street, off Jefferson Highway in Harahan, near the Huey P Long Bridge. They need volunteers to assist with office tasks, to care for cats and dogs, and to walk dogs in the evenings from") +
      time("5:30") + str("to") + time("8:30") > period +
      str("All volunteers are welcome, and no advance signup is required.") +
      moreEmail("A R new orleans at aol dot com"),
    orgName = Some("Animal Rescue"),
    start = "2021-06-22",
    end = Some("2021-06-22"),
    alert = "2021-06-15",
    sourceNote = "Kathy Fleming <kathylee1145@yahoo.com>"
  )

  Spot(
    "DagsHouse",
    Animal,
    str("Dag's House provides information, education and services for dog owners to improve the quality of life of their dogs.") +
      moreWebAnnounce("More information about their services, or about volunteering with them, is", "D A G S house dot com"),
    orgName = Some("Dag's House"),
    start = "2021-06-22",
    alert = "2022-02-03",
    end = Some("2022-02-20"),
    previousAlerts = Seq("2022-02-21"),
    sourceContacts = "Kathy Fleming <kathylee1145@yahoo.com>"
  )

  Spot(
    "BoxerRescue",
    Animal,
    str("Louisiana Boxer Rescue works to rescue, rehabilitate and find new home for abandoned, neglected and abused boxers.") +
      moreWebAnnounce("More information and volunteer opportunities are",
        "louisiana boxer rescue dot O R G"),
    orgName = Some("Louisiana Boxer Rescue"),
    start = "2021-06-22",
    alert = "2022-02-03",
    sourceNote = "kathy fleming <kathylee1145@yahoo.com>"
  )

  val nomuseumsdotcomblurb: StructText =
    str("The New Orleans Tourism Marketing Corporation maintains a directory of city and area museums online at")
      + online("new orleans museums dot com.")

  Spot(
    "TulaneSpecialCollectionsRegular",
    Museum,
    str("Tulane University's Howard-Tilton Memorial Library offers a Special Collections Division for student and professional research. The Hogan Jazz Archive preserves oral histories, recordings, sheet music, and images, about Jazz in New Orleans.  The Louisiana Collection contains books, pamphlets, maps, sheet music, newspapers, photographs and other historical printed materials from French explorations to the present. The special collections also include the manuscripts department, university archives, rare books and the architectural archives.") + moreWebPhone("special collections dot tulane dot edu", "504/865-5685") + nomuseumsdotcomblurb,
    start = "2021-06-22",
    alert = "2022-02-03",
    sourceURL = "http://www.neworleansmuseums.com/"
  )

  Spot(
    "PrcRegular",
    Museum,
    str("The Preservation Resource Center features both permanent and rotating exhibits on New Orleans architecture and historic neighborhoods.  The") +
      doublequoted("Living With History") +
      str("exhibit features hundreds of professional and amateur photographs of New Orleans residents going about their daily lives in several of New Orleans' fascinating neighborhoods.  The center also provides walking tour brochures highlighting neighborhood restaurants, churches, theaters and more.  The Preservation Resource Center is located at") +
      spellout("923") +
      phonetic("Tchoupitoulas", "CHOP-ih-Too-lus", "ˈʧɑːp.əˌtuː.ləs") +
      str("Street in the Warehouse District.") +
      moreWebPhone("www dot P R C N O dot O R G", "504/581-7032") +
      nomuseumsdotcomblurb,
    start = "2021-06-22",
    alert = "2022-02-03",
    sourceURL = "Kathy Fleming <kathylee1145@yahoo.com>"
  )

  Spot(
    "BeauregardKeyesRegular",
    Museum,
    str("The") +
      phonetic("Beauregard", "BO-regard", "ˈboʊ.ɹəˌɡar") +
      str("Keyes House, built in") +
      date("1826", "y") +
      str("and named for two of its former tenants, is a fine example of a raised center hall house.  Visitors can explore the original furnishings of General") +
      phonetic("Beauregard", "BO-regard", "ˈboʊ.ɹəˌɡar") > comma +
      str("and Mrs Keyes' collections of antiques.") +
      morePhone("504/523-7275") + nomuseumsdotcomblurb,
    start = "2021-06-22",
    alert = "2022-02-03",
    sourceURL = "http://www.neworleansmuseums.com/"
  )

  Spot(
    "DegasHouseRegular",
    Museum,
    str("The Edgar") +
      phonetic("Degas", "day-GAH", ".deɪˈɡɑː") +
      str("House was the home of the impressionist painter during his time in New Orleans, and is the only known home or studio of") +
      phonetic("Degas", "day-GAH", ".deɪˈɡɑː") +
      str("that is open to the public. Tours of the home are conducted by appointment only, include a viewing of the award-winning documentary") +
      emph(phonetic("Degas", "day-GAH", ".deɪˈɡɑː") +
        str("in New Orleans, A Creole Sojourn,")) +
      str("and support the Edgar") +
      phonetic("Degas", "day-GAH", ".deɪˈɡɑː") +
      str("Foundation.") +
      moreWebPhone("www dot d e g a s house dot com", "504/821-5009") +
      nomuseumsdotcomblurb,
    start = "2021-06-22",
    alert = "2022-02-10",
    sourceURL = "http://www.neworleansmuseums.com/"
  )

  Spot(
    "PitotHouseRegular",
    Museum,
    str("The") +
      phonetic("Pitot", "pee-TOE", ".piːˈtoʊ") +
      str("House Museum on") +
      phonetic("Bayou", "BUY-you", ".baɪˈjuː") +
      str("Saint John is an eighteenth-century Creole colonial building, and was the home of New Orleans first mayor.  The museum has been restored to highlight its distinctive construction and roof, and is furnished with Louisiana and American antiques from the early") +
      date("1800s", "y") > period +
      str("Visitors do need an appointment to tour the home.") +
      moreWebPhoneAnnounce("More information and appointments are", "www dot P I T O T house dot O R G", "504/482-0312") +
      nomuseumsdotcomblurb,
    start = "2021-06-22",
    end = Some("2022-10-31"),
    alert = "2022-02-10",
    sourceURL = "http://www.neworleansmuseums.com/"
  )

  Spot(
    "PitotHouseRegular2",
    Museum,
    str("The") +
      phonetic("Pitot", "pee-TOE", ".piːˈtoʊ") +
      str("House Museum on") +
      phonetic("Bayou", "BUY-you", ".baɪˈjuː") +
      str("Saint John is an eighteenth-century Creole colonial building, and was the home of New Orleans first mayor.  The museum has been restored to highlight its distinctive construction and roof, and is furnished with Louisiana and American antiques from the early") +
      date("1800s", "y") > period +
      str("Visitors do need an appointment to tour the home.") + moreWebPhoneAnnounce("More information and appointments are", "P I T O T house dot O R G", "504/482-0312") + nomuseumsdotcomblurb,
    start = "2022-10-31",
    alert = "2024-04-15",
    sourceURL = "http://www.neworleansmuseums.com/"
  )

  Spot(
    "JazzParkRegular",
    Museum,
    str("In a city famous for jazz halls and great music, there is a place we can go to learn how New Orleans came to be the birthplace of jazz.  The New Orleans Jazz National Historical Park preserves information and resources related to the beginnings and progressions of jazz in New Orleans. The visitor center is located at") +
      spellout("916") +
      str("North Peters Street in the French Quarter.") +
      moreWebPhone("www dot N P S dot gov slash jazz", "504/589-4806") +
      nomuseumsdotcomblurb,
    start = "2021-06-22",
    alert = "2022-02-10",
    sourceURL = "http://www.neworleansmuseums.com/"
  )

  Spot(
    "DDayMuseumRegular",
    Museum,
    str("New Orleans is home to the country's official World War 2 Museum.  Permanent exhibits tell the story of both fronts of the war from both the military and civilian points of view. Temporary exhibits and lectures delve more deeply into aspect of that era.") +
      moreWebPhone("national w w 2 museum dot O R G", "504/527-6012") +
      nomuseumsdotcomblurb,
    start = "2021-06-22",
    alert = "2022-02-10",
    sourceURL = "http://www.neworleansmuseums.com/"
  )

  Spot(
    "AmericanItalianMuseumRegular",
    Museum,
    str("The American-Italian Renaissance Foundation's Museum tells the story of the cultural contributions of one of New Orleans' more often-overlooked immigrant groups --- from procedures in anesthesiology which are still in use today, to the tradition of the Saint Joseph's Day altar.  The Museum is located at") +
      spellout("537") +
      str("South Peters in the Warehouse District.") +
      moreWebPhone("www dot A I R F dot O R G", "504/522-7294") +
      nomuseumsdotcomblurb,
    start = "2021-06-22",
    alert = "2022-02-10",
    sourceURL = "http://www.neworleansmuseums.com/"
  )

  Spot(
    "PharmacyMuseumRegular",
    Museum,
    str("The New Orleans Pharmacy Museum is the largest and finest pharmaceutical collection in the United States.  Housed in the French Quarter, in the") +
      date("1823", "y") +
      str("apothecary of America's first licensed pharmacist, the Museum contains a collection of 19th-century pharmacy and medical artifacts including an exhibition on epidemics in New Orleans.") + moreWebPhone("www dot pharmacy museum dot O R G", "504/565-8027") + nomuseumsdotcomblurb,
    start = "2021-06-22",
    alert = "2024-03-15",
    sourceURL = "http://www.neworleansmuseums.com/"
  )

  Spot(
    "LongueVueHouseRegular",
    Museum,
    phonetic("Longue Vue", "long view'' or ``long voo", "loʊnɡˈvuː") +
      str("House is one of the few surviving examples of Country Era Place homes in the south, and has been restored in the style of the early 20th Century.  The house and its extensive gardens, as well as the home's Discovery Gardens, a hands-on learning environment for children, are part of the home's public tour.") +
      moreWebPhone("www dot L O N G U E V U E dot com", "504/488-5488") +
      nomuseumsdotcomblurb,
    start = "2021-06-22",
    alert = "2024-05-15",
    sourceURL = "http://www.neworleansmuseums.com/"
  )

  Spot(
    "DDayMuseumLivingHistoryCorps",
    Museum,
    str("The National World War 2 Museum's World War II re-enactors, collectively known as the Living History") +
      phonetic("Corps", "core", "kɑːɹ") > comma +
      str("are local volunteers who enrich visitors' museum experience with their wealth of information and stories. Wearing the uniforms and carrying the equipment of both the Allied and Axis forces, they share their knowledge about the day-to-day lives of military men and women, and the broader lessons of World War 2.  Living History") +
      phonetic("Corps", "core", "kɑːɹ") +
      str("events are open to the public.") +
      moreWebPhone(
        "national W W 2 museum dot O R G slash calendar",
        "504/527-6012 extension 333"),
    start = "2021-06-22",
    alert = "2022-02-10",
    sourceURL = "http://www.nationalww2museum.org/calendar/"
  )

  Spot(
    "AmistadRegular",
    Museum,
    str("The Amistad Research Center houses the country's largest collection of manuscripts about African Americans, race relations and civil rights.  This center is a research resource for historians, novelists, and individual pursuing information about their family's history.  Guided tours are available, but must be scheduled at least two weeks in advance.") +
      moreWebPhone("www dot tulane dot edu slash tilde amistad", "504/865-5535") +
      nomuseumsdotcomblurb,
    start = "2021-06-22",
    alert = "2022-02-10",
    sourceURL = "http://www.neworleansmuseums.com/"
  )

  Spot(
    "TulaneNaturalHistoryRegularB",
    Museum,
    str("The Tulane Museum of Natural History houses extensive collections of amphibians, invertebrates, fish, birds, mammals, reptiles and fossils.  The Museum does not keep public open hours, but individual appointments and arrangements for school tours and scholarly researchers can be made.") +
      moreWebPhone("www dot museum dot tulane dot edu", "504/394-1711"),
    start = "2021-06-22",
    alert = "2022-02-17",
    sourceURL = "http://www.neworleansmuseums.com/"
  )

  Spot(
    "HistoricNewOrleansVisitor",
    Museum,
    str("The Historic New Orleans Collection is dedicated to presenting and preserving the culture of this area. Details about the its current exhibitions, original books, research center, and museum experience are available at") +
      online("H N O C dot O R G") > period,
    start = "2022-09-01",
    alert = "2023-12-17",
    sourceContacts = "Teresa Devlin <Teresa.Devlin@hnoc.org>"
  )

  Spot(
    "HistoricNewOrleansResearcher",
    Museum,
    str("The Historic New Orleans Collection is a nonprofit museum dedicated to documenting the history and culture of New Orleans and the Gulf South. In addition to operating a public history museum, The H-N-O-C runs the Williams Research Center, a non-lending facility where researchers of all levels can access primary resources from the institution's holdings. The website")
      + online("H N O C dot org slash research")
      + str("offers a link to the online catalog as well as information on scheduling appointments.")
      + morePhoneEmail("504/523-4662", "reference at H N O C dot org")
      + nomuseumsdotcomblurb,
    start = "2022-09-12",
    alert = "2024-02-17",
    sourceURL = "http://www.neworleansmuseums.com/"
  )

  Spot(
    "HistoricNewOrleansRegular",
    Museum,
    str("The Historic New Orleans Collection houses primary sources including manuscripts and documents, literary and artistic treasures, and other artifacts showcasing the numerous cultures that shaped New Orleans from the 18th century to the present.  Along with public exhibition galleries and a museum shop, the Collection offers A research guide and photocopying services for scholarly and historic research.") +
      moreWebPhone("www dot H N O C dot O R G", "504/523-4662") +
      nomuseumsdotcomblurb,
    start = "2021-06-22",
    alert = "2022-02-17",
    end = Some("2022-09-11"),
    sourceURL = "http://www.neworleansmuseums.com/"
  )

  Spot(
    "OgdenAfterHours",
    Museum,
    str("Every Thursday evening, The Ogden Museum of Southern Art presents") +
      emph("Ogden After Hours,") +
      str("music in the atrium of the museum.  During these concerts, the museum also offers art activities for kids.") +
      moreWebPhone("www dot ogden museum dot O R G", "504/539-9600") +
      nomuseumsdotcomblurb,
    start = "2021-06-22",
    alert = "2022-02-17",
    end = Some("2021-11-21"),
    sourceURL = "http://www.neworleansmuseums.com/"
  )

  Spot(
    "JeanLafitteRegular",
    Museum,
    str("The") +
      phonetic("Jean Lafitte", "zhawn-la-FEET", "ʒə.læˈfiːt") +
      str("National Historical Park and Preserve consists of six sites in Louisiana, three of which are in the metropolitan New Orleans area: the Laura C. Hudson Visitor Center in the French Quarter, the Barataria Preserve, and the Chalmette Battlefield and National Cemetery.") +
      moreWebPhoneAnnounce(
        "More information about these sites is",
        "www dot N P S dot gov slash J E L A", "504/589-3882") +
      nomuseumsdotcomblurb,
    start = "2021-06-22",
    alert = "2023-09-15",
    sourceURL = "http://www.neworleansmuseums.com/"
  )

  Spot(
    "CabildoRegular",
    Museum,
    str("The Cabildo is one of the most historically significant buildings in America.  It was the seat of the Spanish Colonial government, the site of the Louisiana Purchase transfer in") +
      date("1803", "y") > comma +
      str("and a home to the Louisiana State Supreme Court.  Now, the Cabildo is a public museum showcasing the rich and colorful textures of Louisiana's history.") + moreWebPhone("L S M dot C R T dot state dot L A dot U S", "800/568-6968") + nomuseumsdotcomblurb,
    start = "2021-06-22",
    alert = "2022-02-17",
    sourceURL = "http://www.neworleansmuseums.com/"
  )

  Spot(
    "ChildrensMuseumRegular",
    Museum,
    str("The Louisiana Children's Museum offers more than 30,000 square feet of hands-on, interactive exhibits that invite and engage children and families as they explore art, music, science, math, and health, and role-playing environments. The museum is located at 15 Henry Thomas Drive in City Park, is open seven days a week in the summer, and closes Mondays during the school year.") + moreWebPhone("www dot L C M dot O R G", "504/523-1357") + nomuseumsdotcomblurb,
    start = "2021-06-22",
    alert = "2022-02-17",
    sourceURL = "http://www.neworleansmuseums.com/"
  )

  Spot(
    "BackstreetRegular",
    Museum,
    str("The Backstreet Cultural Museum is home to an amazing assortment of memorabilia of Mardi Gras, jazz funerals and other traditions found only in New Orleans, including the city's largest collection of Mardi Gras Indian costumes.") + moreWebPhoneAnnounce("Directions to the museum and more information are", "www dot back street museum dot O R G", "504/522-4806") + nomuseumsdotcomblurb,
    start = "2021-06-22",
    alert = "2022-02-17",
    sourceURL = "http://www.neworleansmuseums.com/"
  )

  Spot(
    "AfAmMuseumRegular",
    Museum,
    str("The New Orleans African American Museum is dedicated to protecting, preserving, and promoting through education the history, art, and communities of African Americans in New Orleans and the African diaspora.  Five restored building over a full city block in the") +
      phonetic("Trème", "TREHM-ay", "tɹəˈmeɪ") +
      str("host permanent and changing exhibits, and a serene garden.  Tour groups, school groups and individuals are welcomed by appointment only.") + moreWebPhoneAnnounce("Information and appointments are", "www dot N O A A M dot O R G", "504/566-1136") + nomuseumsdotcomblurb,
    start = "2021-06-22",
    alert = "2022-02-24",
    sourceURL = "http://www.neworleansmuseums.com/"
  )

  Spot(
    "SculptureGardenRegular",
    Museum,
    str("The Besthoff Sculpture Garden occupies five acres of City Park, and is open to the public five days a week, Wednesdays through Sundays.  The collection includes work by some of the great master sculptors of the twentieth century, as well as younger, contemporary sculptors.") +
      moreWeb("www dot N O M A dot O R G") +
      nomuseumsdotcomblurb,
    start = "2021-06-22",
    alert = "2022-02-24",
    sourceURL = "http://www.neworleansmuseums.com/"
  )

  Spot(
    "OgdenRegular",
    Museum,
    str("The Ogden Museum of Southern Art is home to the most comprehensive collection of southern art in the world, spanning fifteen states and four centuries.  The Ogden features special programming for visitors of all ages, plus weekly evening gallery openings and music performances.") + moreWebPhone("www dot ogden museum dot O R G", "504/539-9600") + nomuseumsdotcomblurb,
    start = "2021-06-22",
    alert = "2022-02-24",
    sourceURL = "http://www.neworleansmuseums.com/"
  )

  Spot(
    "NOMAregular",
    Museum,
    str("The New Orleans Museum of Art is the premier art museum of the Gulf South region.") +
      phoneticIPA("NOMA's", "ˈnoʊ.məz") +
      str("broad collection includes a notable collection of Fabergé eggs and treasures, and the Latin American Colonial collection.") + moreWebPhone("www dot N O M A dot O R G", "504/488-2631") + nomuseumsdotcomblurb,
    start = "2021-06-22",
    alert = "2022-02-24",
    sourceURL = "http://www.neworleansmuseums.com/"
  )

  Spot(
    "DibollRegular",
    Museum,
    str("The Collins C Diboll Art Gallery is a small museum dedicated to artifacts of Belgian Congo plus rotating exhibits, including exhibits of Loyola student and faculty work.  The gallery is located in the Monroe Library on Loyola's campus.") + moreWebPhone("www dot L O Y N O dot edu", "504/861-5456") + nomuseumsdotcomblurb,
    start = "2021-06-22",
    alert = "2022-02-24",
    sourceURL = "http://www.neworleansmuseums.com/"
  )

  Spot(
    "NewcombArtRegular",
    Museum,
    str("The Newcomb Art Gallery showcases a permanent collection of nineteenth- and twentieth-century pieces produced at Newcomb College, in addition to hosting quarterly traveling exhibitions featuring mostly contemporary artists.  The world-renowned Newcomb Pottery section represents the largest area of holdings at the museum and features several rare exhibition-only and early experimental pieces.") + moreWebPhone("www dot newcomb art gallery dot com", "504/865-5328") + nomuseumsdotcomblurb,
    start = "2021-06-22",
    alert = "2022-02-24",
    sourceURL = "http://www.neworleansmuseums.com/"
  )

  Spot(
    "CACregular",
    Museum,
    str("The Contemporary Arts Center is a multidisciplinary arts center presenting an array of programs encompassing the visual arts, music, dance and drama, all celebrating the arts of our time.") + moreWebPhone("www dot C A C N O dot O R G", "504/528-3805") + nomuseumsdotcomblurb,
    start = "2021-06-22",
    alert = "2024-05-15",
    sourceURL = "http://www.neworleansmuseums.com/"
  )

  Spot(
    "HermannGrimaHouseRegular",
    Museum,
    str("Located in the heart of the historic French Quarter, the Hermann-Grima House is thought to be the finest example of American architecture in the area.  The mansion has accurately restored to depict the gracious lifestyle of a wealthy Creole family from") +
      date("1830", "y") +
      str("to") +
      date("1860", "y") > period +
      str("Visitors can tour the original stables, the outdoor kitchen with the open hearth for cooking, and a meticulously restored courtyard with citrus trees and antique roses.") +
      moreWebPhone("www dot H G G H H dot O R G", "504/525-5661") +
      nomuseumsdotcomblurb,
    start = "2021-06-22",
    alert = "2023-06-15",
    sourceURL = "http://www.neworleansmuseums.com/"
  )

  Spot(
    "GallierHouseRegular",
    Museum, (
      str("") +
      phonetic("Gallier", "GAL-yer", "ˈɡæl.jəɹ") +
      str("House, built in") +
      date("1857", "y") > comma +
      str("was described as one of the best small museums in the country by the") + emph("New York Times.") + str("The house is a well-preserved example of progressive design, and is decorated throughout the year to reflect period seasonal styles.")
        + moreWebPhoneAnnounce(
          "More information about Gallier House is",
          "www dot H G G H H dot O R G", "504/525-5661")
        + nomuseumsdotcomblurb
    ),
    start = "2021-06-22",
    alert = "2023-06-15",
    sourceURL = "http://www.neworleansmuseums.com/"
  )

  Spot(
    "ShalamarWritingWorkshop",
    Rare,
    str("Shalamar Publishing holds writing workshops to offer feedback and advice to local writers.  Workshops are open to the public, and are held the first Sunday of each month at six") + pm + str("at the CC's Coffee at 3647 Veterans Memorial Boulevard in Metairie."),
    start = "2021-06-22",
    end = Some("2021-06-22"),
    alert = "2021-06-15",
    previousAlerts = Seq("2021-10-25"), // Responded to pull
    orgName = "Shalamar Publishing",
    sourceContacts = "Deborah Dixon <deboracracy@gmail.com>"
  )

  Spot(
    "MicrowaveBoilingWater", Rare,
    str("Boiling water in a microwave oven may seem easy and convenient --- but serious burns are never easy, or convenient.  Water heated by microwaves can") +
      emph("superheat") > comma +
      str("leading to an explosion of steam and hot water when the container is picked up.  You can keep water from superheating by heating it in a wide-mouthed container like a mixing cup, never a narrow-mouthed container like a jar, and adding a wooden popsicle stick or toothpick to the water, to give bubbles a place to form.  Remember: no matter much of a hurry you're in, you don't have time for superheated microwave water scalding."),
    start = "2021-06-22",
    alert = "2029-07-15"
  )

  Spot(
    "JobSeekersNOLA",
    Rare,
    str("The Job Seekers Alliance is a networking group for New Orleans area individuals looking for work in the non-profit sector.  The group meets on the first and third Friday of each month from ten to eleven") + am + str("at") +
      spellout("1824") +
      str("Oretha Castle Haley Boulevard.") + moreWeb("J S A nola at gmail dot com"),
    orgName = Some("Job Seekers Alliance"),
    start = "2021-06-22",
    alert = "2022-02-24",
    previousAlerts = Seq("2022-02-20"),
    copresent = "the New Orleans Job Seekers Alliance",
    sourceContacts = "Heather Mack <jsanola@gmail.com>"
  )

  Spot(
    "RockTheEarth",
    Rare,
    str("Some of the best music in the world invokes a sense of time and place in the listener.  Today many of our planet's special places are under increased stress and your help is needed to protect the areas that we all cherish.  Rock the Earth and their musical partners are working hard in the name of our most precious natural places.") +
      moreWebAnnounce("More information about their work and how you can help are", "rock the earth dot O R G"),
    orgName = Some("Rock the Earth"),
    start = "2021-06-22",
    alert = "2022-03-04",
    sourceContacts = "glennf@rocktheearth.org"
  )

  Spot(
    "AgendaForChildren",
    Rare,
    str("Agenda for Children advocates for children's programs in Louisiana for education, health, adoption and foster care, and the justice system. " +
      moreWebAnnounce(
        "More information, including how you can support their mission, is",
        "agenda for children dot O R G")),
    orgName = Some("Agenda for Children"),
    start = "2021-06-22",
    alert = "2022-03-04",
    sourceContacts = "Erica Severson <SeversonE@peteramayer.com>"
  )

  Spot(
    "SBAbeforeDistaster", Rare,
    str("Before a disaster strikes, there are things you can do to protect yourself.  The") +
      spellout("US") +
      str("Small Business Administration encourages homeowners and small businesses to develop a disaster preparedness plan.  Preparation includes: having a battery operated radio on hand to monitor news and weather reports;  identifying possible hazards;  familiarizing yourself with escape routes for use in an emergency;  keeping phone numbers handy; and saving copies of financial records offsite to help speed up the recovery process.") +
      moreWeb("S B A dot gov"),
    orgName = Some("Small Business Administration"),
    sourceContacts = "Aaron Viles <aaron@healthygulf.org>",
    copresent = "U.S.\\ Small Business Administration",
    start = "2021-06-22",
    alert = "2022-07-15"
  )

  Spot(
    "Eracism",
    Rare,
    str("Eracism is the slogan of the group ERACE, which was formed in New Orleans in") +
      date("1993", "y") > period +
      str("Their mission is to seek ways through person-to-person communication to show that they are committed to treating fellow human beings of all colors with love and respect.") + morePhoneAnnounce("More information about ERACE, its discussion meeting schedule, or bumper stickers is", "504/866-1163"),
    orgName = Some("Eracism"),
    start = "2021-06-22",
    end = Some("2024-03-31"),
    alert = "2022-07-15"
  )

  Spot(
    "Eracism2",
    Rare,
    str("Eracism is the slogan of the group ERACE, which was formed in New Orleans in")
      + date("1993", "y") > period
      + str("Their mission is to seek ways through person-to-person communication to show that they are committed to treating fellow human beings of all colors with love and respect.")
      + moreWeb("E R A C I S M new orleans dot O R G"),
    orgName = Some("Eracism"),
    start = "2021-04-01",
    alert = "2027-07-15"
  )

  Spot(
    "IrsWebSite",
    Taxtime,
    str("The") +
      spellout("IRS") +
      str("website,") +
      online("I R S dot gov") + str("is a one-stop shop for an array of tax information. You can even prepare and file your federal tax return through") +
      doublequoted("Free File") > comma +
      str("a service offered by") +
      spellout("IRS") +
      str("and its partners who offer tax preparation software and electronic filing as a public service. The site also includes answers to common tax questions, and lets you check the status of your refund. The") +
      spellout("IRS") +
      str("website is") + online("I R S dot gov") > period,
    orgName = Some("IRS"),
    start = "2021-06-22",
    alert = "2022-07-15"
  )

  Spot(
    "IrsMilitaryHelp", Taxtime,
    str("Free tax return preparation assistance is available for eligible military members and their spouses. Volunteers at military VITA sites are trained to address military-specific tax issues, such as combat zone tax benefits and the Earned Income Tax Credit guidelines. More information is available in") +
      spellout("IRS") +
      str("Publication 3, the") + emph("Armed Forces' Tax Guide") >
      ", available on the" +
      spellout("IRS") +
      str("web site") + online("I R S dot gov,") + str("or by phone, 800/TAX-FORM, that's 800/829-3676."),
    orgName = Some("IRS"),
    start = "2021-06-22",
    alert = "2022-07-15"
  )

  Spot(
    "IrsElderlyAssistance",
    TaxAlways,
    str("Free tax preparation is available through the Volunteer Income Tax Assistance and Tax Counseling for the Elderly programs in many communities. Volunteer return preparation programs are provided through partnerships between the") +
      spellout("IRS") +
      str("and community-based organizations. They offer free help in preparing simple tax returns for low-to-moderate income taxpayers. The New Orleans office is located at") +
      spellout("1555") +
      phonetic("Poydras", "POI-druhss") +
      str("Street, and their phone number is 504/558-3344.") +
      moreWebPhoneAnnounce(
        "More information including other locations is available",
        "I R S dot gov", "800/906-9887"),
    orgName = Some("Volunteer Income Tax Assistance"),
    start = "2021-06-22",
    alert = "2022-07-15"
  )

  Spot(
    "IrsPhoneNumbers",
    TaxAlways,
    str("The") +
      spellout("IRS") +
      str("operates a number of phone services for taxpayers. Staff will answer your federal tax questions on the tax help line for individuals,") +
      phone("800/829-1040") > period +
      str("Pre-recorded messages covering various tax topics, plus automatic checking of the status of your refund, are available at") +
      phone("800/829-4477") > period +
      str("You can order forms, instructions and publications at") +
      phone("800/829-3676") > period +
      str("Finally, TTY and TDD users may call") +
      phone("800/829-4059") +
      str("for tax questions and ordering forms and publications."),
    orgName = Some("IRS"),
    start = "2021-06-22",
    alert = "2022-07-15"
  )

  Spot(
    "IrsHelpInPerson",
    TaxAlways,
    str("If you believe your tax issue cannot be handled online or by phone, and you want face-to-face assistance, you can find help at a local") +
      spellout("IRS") +
      str("Taxpayer Assistance Center. The New Orleans office is located at") +
      spellout("1555") +
      phonetic("Poydras", "POI-druhss") +
      str("Street, and their phone number is") +
      phone("504/558-3344") > period +
      str("Other locations, business hours and an overview of services are available online at") +
      online("I R S dot gov,") +
      str("under the") +
      doublequoted("Individuals") +
      str("tab, and") +
      doublequoted("Contact My Local Office") +
      str("link."),
    orgName = Some("IRS"),
    start = "2021-06-22",
    alert = "2022-07-15"
  )

  Spot(
    "IrsScamAwareness",
    TaxAlways,
    str("The") +
      spellout("IRS") +
      str("warns taxpayers to be wary of tax scams. They issue an annual") +
      doublequoted("Dirty Dozen") +
      str("tax scams list ranging from identity theft to return preparer fraud. More information, including warning signs and new scams, are available by search on the") +
      spellout("IRS") +
      str("website,") + online("I R S dot gov") > period,
    orgName = Some("IRS"),
    start = "2021-06-22",
    alert = "2022-07-15"
  )

  Spot(
    "FemaApp",
    StormPrep,
    str("FEMA, the Federal Emergency Management Agency, now has an app for hurricane season readiness.  You can get alerts from the National Weather Service, plus safety reminders, emergency checklists, and information about shelters.  More information about the app is available at") + online("fema dot gov slash mobile hyphen app") > period,
    orgName = Some("FEMA"),
    copresent = "FEMA",
    start = "2021-07-12",
    alert = "2022-05-15"
  )

  Spot(
    "FemaStormDocPrep",
    StormPrep,
    str("If you need to evacuate for a hurricane, having important documents ready to go can help get your recovery process started quickly and efficiently in the worst cases.  Keep important papers in a fireproof, waterproof box or safe in your home next to the emergency kit. Store copies in an alternate location such as a safe deposit box, work place or trusted friend or family member's home.  You can make electronic copies and keep them on a flash drive or CD in your emergency kit, email copies to yourself, or upload them to a cloud storage service.  Other important documents to include are: birth certificates, passports, Social Security cards; insurance policies; deed, mortgage, lease and loan papers; lists of medications, allergies and medical equipment; photos of valuable belongings you may want to include in an insurance claim; and contact information for doctors, relatives, creditors and utilities.  More information on preparing for hurricanes is available at") + online("ready dot gov slash hurricanes") > period,
    orgName = Some("FEMA"),
    copresent = "FEMA",
    start = "2021-07-12",
    alert = "2022-05-15"
  )

  Spot(
    "FemaContractorWarning",
    StormPrep,
    str("After a hurricane or other natural disaster, survivors should ber wary of post-disaster fraud and scams. Attempts to scam residents can be made over the phone, by mail or email, through the internet or in person.  It is important to remain alert.  If an offer sounds too good to be true, it should be questioned.  To find out if a potential contractor is licensed to work in Louisiana, check the website of the Louisiana State Licensing Board for Contractors at") +
      online("L S L B C dot louisiana dot gov,") + str("or call them at") +
      phone("225/765-2301") > period,
    orgName = Some("FEMA"),
    copresent = "FEMA, the Louisiana Governor’s Office of Homeland Security and Emergency Preparedness",
    start = "2021-07-12",
    alert = "2022-05-15"
  )

  Spot(
    "TempHousingStormTwentyTwo",
    StormPrep,
    str("FEMA encourages survivors living in temporary housing to prepare for hurricanes.  Trailers and other temporary housing units are not safe in a hurricane, so occupants in state and FEMA housing units should plan ahead in case a storm arrives.  Make your personal emergency plan now so if your local emergency management officials tell you to evacuate you will know what to take, who will drive and where you will go. Take pets with you if you evacuate. Pack all medications you will need while you are evacuated.  And pay attention to directions for evacuation routes.") +
      online("get a game plan dot O R G"),
    orgName = Some("FEMA"),
    start = "2022-06-09",
    end = Some("2022-09-30"),
    alert = "2024-05-15"
  )

  Spot(
    "BusinessStormPrepGovernor",
    StormPrep,
    str("Is your business ready to recover after the damaging winds and floods of a hurricane? The Governor's Office of Homeland Security and Emergency Preparedness has a checklist for businesses so that they can stay in business despite a storm. Their web site is") +
      online("get a game plan dot O R G"),
    orgName = Some("Governor's Office of Homeland Security"),
    start = "2021-06-22",
    alert = "2022-07-15"
  )

  Spot(
    "Evacuteer",
    StormPrep,
    str("Thousands of New Orleanians may need help evacuating this hurricane season.  You can assist by volunteering with Evacuteer, to help get people out of harm's way in case of an evacuation.  Evacuteer hosts one-hour volunteer trainings at libraries and community spaces throughout the city.") +
      moreWebAnnounce("More information, and registration for training, are",
        "evacuteer dot O R G.") +
      str("That's") +
      online("E V A C U T E E R dot O R G") >
      str("."),
    orgName = Some("Evacuteer"),
    start = "2021-06-22",
    alert = "2022-03-04",
    groupGainMultiplier = 1.2
  )

  Spot(
    "EvacuteerService",
    StormPrep,
    str("New Orleans' City-Assisted Evacuation provides residents with a free ride to a shelter during a mandatory evacuation. They also provide a ride home once it is safe to return.  Evacuteer volunteers will help residents and make sure that families, including pets, stay together. " +
      moreWeb("ready dot nola dot gov")),
    orgName = Some("Evacuteer"),
    start = "2021-06-22",
    alert = "2022-03-04",
    groupGainMultiplier = 1.5
  )

  Spot(
    "HurricaneContraflowAlertB",
    StormPrep,
    str("Do you know the way out of town in a hurricane evacuation?  When a storm is approaching, our interstate highways are rerouted for") +
      emph("contraflow") >
      comma +
      str("where all lanes head away from the coast.  During contraflow it's important to know where you're going, because there may be few exits from the highway once you enter.  More information about contraflow routes is available on the Evacuation Guide for Southeast Louisiana, available from the Governor's Office of Homeland Security and Emergency Preparedness website,") +
      online("get a game plan dot O R G."),
    orgName = Some("GOHEP"),
    start = "2021-06-22",
    alert = "2022-03-04",
    groupGainMultiplier = 1.2
  )

  Spot(
    "HurricaneDisabilitiesB",
    StormPrep,
    str("People with disabilities can reduce the fear, panic, and inconvenience that comes with hurricane evacuation by planning ahead.   The website of the Governor's Office of Homeland Security and Emergency Preparedness, at") +
      online("get a game plan dot O R G") +
      str("has a list of precautions and preparations."),
    orgName = Some("GOHEP"),
    start = "2021-06-22",
    alert = "2022-03-11",
    groupGainMultiplier = 1.2
  )

  Spot(
    "HurricanePetsAlertB",
    StormPrep,
    str("You may have a hurricane evacuation plan, but can your pet come along?   The website of the Governor's Office of Homeland Security and Emergency Preparedness has information about planning for your pets as a storm approaches.") + moreWeb("get a game plan dot O R G"),
    orgName = Some("GOHEP"),
    start = "2021-06-22",
    alert = "2022-03-11",
    groupGainMultiplier = 1.2
  )

  Spot(
    "HurricaneShelterAlert",
    StormPrep,
    str("Do you have somewhere to go when a hurricane approaches?  Just because you can't afford a hotel, doesn't mean you have to ride out a storm.  For information about shelter options, get the Evacuation Guide for Southeast Louisiana from the Governor's Office of Homeland Security and Emergency Preparedness.  You can download it from their website,") +
      online("get a game plan dot O R G") > period,
    orgName = Some("GOHEP"),
    start = "2021-06-22",
    alert = "2022-03-11",
    groupGainMultiplier = 1.2
  )

  Spot(
    "SEfloodProtectWebsiteB",
    StormPrep,
    str("The Southeast Louisiana Flood Protection Authority-West, which provides flood protection for nearly all land on the west bank of the Mississippi River in Jefferson and Orleans parishes, provides information to the public via its web site,") +
      online("S L F P A W dot O R G.") +
      str("The site provide information to the public about flood incidents, ongoing projects, and permit guidelines. Again, the URL is") +
      online("S L F P A W dot O R G") > period,
    orgName = Some("Flood Protection Authority"),
    start = "2021-06-22",
    alert = "2022-07-15"
  )

  Spot(
    "StormPrepCityWebSiteE",
    StormPrep,
    str("NOLA Ready is the City of New Orleans's online Emergency Alert System.  Its web site") +
      online("ready dot NOLA dot gov") +
      str("helps the citizens of New Orleans get themselves, their homes and their businesses prepared for hurricane season. Information includes supplies citizens should have on hand, how to form an emergency plan, what to include in an emergency kit and to-go bag, how to get out of town, and resources for pet-owners and business owners. In the event of emergency,") +
      online("ready dot NOLA dot gov") +
      str("will provide fast, accurate updates."),
    orgName = Some("NOLA Ready"),
    start = "2021-06-22",
    alert = "2022-03-11",
    groupGainMultiplier = 1.65
  )

  Spot(
    "StormTaxPrepIRS", StormPrep,
    str("The") +
      spellout("IRS") +
      str("offers resources to help both individuals and businesses prepare for the disruption a hurricane can bring. Their disaster loss workbooks can help you compile a room-by-room list of personal belongings and business equipment. Those workbooks and more information are available on their website at") + online("I R S dot gov slash publications slash p 584") + str("and") + online("publications slash p 584 b") > period,
    orgName = Some("IRS"),
    start = "2021-06-22",
    alert = "2022-03-11"
  )

  Spot(
    "TaxPrepElectronicRecordsIRS",
    StormPrep,
    str("The") +
      spellout("IRS") +
      str("encourages you to be prepared for the year's hurricane season. Electronic records are an easy way to make your financial history portable. You can scan important records such as W-2s, tax returns and other paper documents onto an electronic format, and carry them  on a CD or DVD or other electronic storage device.You can also photograph or videotape the contents of your home and/or business, especially items of greater value, for tax and insurance purposes. Remember to store these electronic records away from areas at risk, or to bring them with you when you evacuate."),
    orgName = Some("IRS"),
    start = "2021-06-22",
    alert = "2022-03-11"
  )

  Spot(
    "HolidayPetChocolate",
    Holiday,
    str("Holiday cooking and gifts often include chocolate.  However, chocolate is very toxic to pets and can cause digestive, heart and brain disease including vomiting, rapid heart rate, high blood pressure, and seizures.  Exposure to chocolate can be life-threatening to pets, and treatments are expensive.  The best way to protect pets is to limit their risk of exposure at home by making sure that chocolate snacks are out of their reach. More information about poisons and pets is available from the ASPCA's web site,") + online("A S P C A  dot O R G slash pet hyphen care slash animal hyphen poison hyphen control") > period,
    orgName = Some("ASPCA"),
    sourceContacts = "Ginger Guttner <ginger@lsu.edu>",
    start = "2021-06-22",
    alert = "2024-12-01",
    previousAlerts = Seq("2022-02-20"),
    copresent = "LSU School of Veterinary Medicine"
  )

  Spot(
    "HolidayPetElectricity",
    Holiday,
    str("Holiday decorations often involve more electrical cords, and pets may be attracted to them.  Electric shock from chewed cords may have devastating consequences.  Pet owners can try to reduce pets' access to cords, watch pets to make sure they are not chewing on new cords.  Some pets may try to eat batteries, which should also be stored safely out of their reach.  LSU's Veterinary Teaching Hospital will remain open 24 hours daily through the holidays.") + moreWebPhone("L S U dot E D U slash vet med", "225/578-9600"),
    orgName = Some("Veterinary Teaching Hospital"),
    sourceContacts = "Ginger Guttner <ginger@lsu.edu>",
    start = "2021-06-22",
    alert = "2026-12-01",
    copresent = "LSU School of Veterinary Medicine"
  )

  Spot(
    "HolidayPetWrappers", Holiday,
    str("Candy wrappers, aluminum foil, plastic wrap, or ribbons can lead to serious problems if eaten by dogs or cats. Tinsel is particularly enticing to cats.  When ingested in sufficient quantities, it binds into a rope that can cause severe intestinal obstruction and require surgical treatment. Any small decoration or toy poses a swallowing hazard. If a child can choke on small toys or parts, then so can the family dog or cat.  LSU's Veterinary Teaching Hospital will remain open 24 hours daily through the holidays.") + moreWebPhone("L S U dot E D U slash vet med", "225/578-9600"),
    orgName = Some("Veterinary Teaching Hospital"),
    sourceContacts = "Ginger Guttner <ginger@lsu.edu>",
    start = "2021-06-22",
    alert = "2028-12-01",
    previousAlerts = Seq("2022-02-20"),
    copresent = "LSU School of Veterinary Medicine"
  )

  Spot(
    "HolidayPetTableFood",
    Holiday,
    str("Table food can cause dogs to suffer from acute") +
      phonetic("gastroenteritis", "GAS-tro-En-ter-Eye-tiss") +
      str("or") +
      phonetic("pancreatitis", "PAN-kree-uh-Tie-tiss") > period +
      str("In both diseases, dogs experience severe vomiting, diarrhea, abdominal pain, and listlessness. Bones may obstruct the esophagus, the stomach or the intestine and lead to severe digestive symptoms. Furthermore, grapes, raisins and onions are foods that dogs and cats should not receive. They are toxic to pets and can cause potentially fatal diseases.  Pet owners can avoid these risks by making sure that they and their guests do not feed table food to pets.  More information about poisons and pets is available from the") +
      spellout("ASPCA") +
      str("web site,") +
      online("A S P C A dot O R G slash pet hyphen care slash animal hyphen poison hyphen control") > period,
    orgName = Some("ASPCA"),
    sourceContacts = "Ginger Guttner <ginger@lsu.edu>",
    start = "2021-06-22",
    alert = "2030-12-01",
    previousAlerts = Seq("2022-02-20"),
    copresent = "LSU School of Veterinary Medicine"
  )

  Spot(
    "HolidayPetPlants",
    Holiday,
    str("Over the holidays, many of us will have seasonal plants in the house.  But ornamental plants like") +
      phonetic("poinsettias", "point-SET-uhz") > comma +
      str("mistletoe and holly are toxic to animals.  They can cause upset stomachs or worse symptoms in dogs and cats.  Owners can protect pets by placing them away from where pets go, and by watching for signs that the plants are being eaten.  More information about poisons and pets is available from the") +
      spellout("ASPCA") +
      str("web site,") +
      online("A S P C A dot O R G slash pet hyphen care slash animal hyphen poison hyphen control") > period,
    orgName = Some("ASPCA"),
    sourceContacts = "Ginger Guttner <ginger@lsu.edu>",
    start = "2021-06-22",
    alert = "2032-12-01",
    previousAlerts = Seq("2022-02-20"),
    copresent = "LSU School of Veterinary Medicine"
  )

  Spot(
    "HolidayPetFreeze",
    Holiday,
    str("The weather in December and January can be quite chilly even in Louisiana. Please remember to bring in outside pets overnight if a hard freeze is forecast.  More information about winter pet care is available from the") +
      spellout("ASPCA") +
      str("web site,") +
      online("A S P C A dot O R G") > period,
    sourceContacts = "Ginger Guttner <ginger@lsu.edu>",
    start = "2021-06-22",
    alert = "2034-06-01",
    copresent = "LSU School of Veterinary Medicine"
  )

  Spot(
    "NationalSuicidePrevCarnival",
    Carnival,
    str("The National Suicide Prevention crisis counseling hotline is") +
      phone("800/273-8255") > period +
      str("This number is available both for people who are having suicidal thoughts, and who are concerned about others.  That number again is") +
      phone("800/273-8255") > period,
    orgName = Some("National Suicide Prevention"),
    start = "2021-06-22",
    alert = "2029-07-15",
    boost = 0.45
  )

  Spot(
    "RapeCrisisCarnival",
    Carnival,
    str("The Greater New Orleans Center for Women and Children operates programs to assist victims of domestic violence and sexual assault. Services include a 24 hour confidential crisis line, shelter for victims of domestic violence, limited legal assistance (including referrals), and counseling and advocacy services.  All services are confidential and are provided at no cost by specially trained staff members.  Help is provided both for victims, and for people who want to stop committing violent acts.  Assistance is available by calling") +
      phone("504/837-5400") > period,
    start = "2021-06-22",
    alert = "2022-01-15",
    boost = 0.54
  )

  Spot(
    "SAPHEcarnival",
    Carnival,
    phonetic("SAPHE", "safe", "seɪf") > comma +
      str("Tulane's Sexual Aggression Peer Hotline and Education program, provides the Tulane community with resources, support and education about sexual aggression. If you or someone you know is a victim of sexual assault, relationship violence, stalking, harassment, or exploitation, you can call") +
      phonetic("SAPHE's", "safes", "seɪfs") +
      str("student-operated, confidential hotline at") +
      phone("504/654-9543") > period +
      str("Again, the hotline number is") +
      phone("504/654-9543") > period,
    orgName = Some("SAPHE"),
    start = "2021-06-22",
    alert = "2022-01-15",
    boost = 0.45
  )

  Spot(
    "CityParadeParking",
    Carnival,
    str("The City of New Orleans has special restrictions for parking near parades and for using neutral grounds during carnival season.") +
      moreWeb("nola dot gov"),
    start = "2021-06-22",
    alert = "2022-01-15"
  )

  Spot(
    "SummerInjuryPrevention",
    Summer,
    str("Forty percent of all injury-related emergency room visits --- and forty-two percent of all injury deaths --- happen between May and August.  Tips on how to keep your children active and safe over the summer are available online at") + online("H L C online dot O R G") > period,
    start = "2021-06-22",
    alert = "2022-04-15"
  )

  Spot(
    "DrowningPrevention",
    Summer,
    str("Drowning is a leading cause of accidental death for children under fifteen. Parents should know CPR, have a life preserver on hand, and never let children swim in a pool unsupervised. More summer water safety tips are available online at") +
      online("H L C online dot O R G"),
    start = "2021-06-22",
    alert = "2022-04-15"
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
