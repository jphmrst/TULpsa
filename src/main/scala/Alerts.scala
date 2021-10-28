// PSAs.scala --- (c) 2021 John Maraist
// Part of the WTUL Roster Generator
//
// This file is made available under the GNU GPL version 3; see the
// LICENSE file in this distribution or https://fsf.org/ for more
// details.

// To skip to the actual PSA spots: search for "short-term" or
// "long-term".


package org.maraist.wtulrosters
import java.time.LocalDate
import java.time.format.DateTimeFormatter

object Alerts {

  def printAlerts(): Unit = {
    PsaRosters.init()
    println(s"Reviewing ${PsaLongTermSpots.size} long-term spots")

    val now = LocalDate.now

    val allAlertable = PsaLongTermSpots.all.filter((spot) =>
      spot.end.map(now.isBefore(_)).getOrElse(true) // Still running
      && spot.alert.isBefore(now) // Time to alert
    )
    println(s"${allAlertable.size} spot(s) alertable")

    val emailAlertable =
      allAlertable.filter((spot) => spot.sourceContacts.length > 0)
    println(s"${emailAlertable.size} spot(s) alertable by email")

    val webAlertable = allAlertable.filter((spot) =>
      spot.sourceContacts.length == 0 && spot.sourceURL.length > 0)
    println(s"${webAlertable.size} spot(s) alertable online")

    val otherAlertable = allAlertable.filter((spot) =>
      spot.sourceContacts.length == 0 && spot.sourceURL.length == 0)
    println(s"${otherAlertable.size} other spot(s) alertable")

    for (spot <- otherAlertable) do {
      showAlertHeader(spot)
    }

    for (spot <- webAlertable) do {
      showAlertHeader(spot)
      println("Check online at")
      for (url <- spot.sourceURL) do println("- " + url)
    }

    for (spot <- emailAlertable) do {
      showAlertHeader(spot)
      var toSep = "To: "
      for (to <- spot.sourceContacts) do {
        print(toSep + to)
        toSep = ", "
      }
      println()
      println()
      println("Hi,")
      println()
      println(s"I'm writing about ${spot.orgName.map("a " + _).getOrElse("a")} public service announcement which we are currently running in a long-term rotation on WTUL.  I have a note that this email address is the contact for this announcement, and I'd like to make sure that it is still current.  Please have a look at the text below, and let me know about any updates or changes that we should make to it.")
      println()
      println("Please do respond even if there are no changes — we use these responses to make sure that a group or service is still operating.  Getting a reply to this email is a big step in making sure that an announcement continues to air.")
      println()
      println("Thanks,")
      println("-John")
      println()
      println()
      println(s"PSA code \"${spot.tag}\" (please include this code in your reply)")
      println()
      println(spot.text)
      println()
    }

    println("============================================================")
    println(s"Total alerts: ${webAlertable.size + emailAlertable.size + otherAlertable.size}")
  }

  def showAlertHeader(spot: Spot): Unit = {
    println("============================================================")
    println(spot.tag)
    println(s"Subject: ${spot.orgName.map(_ + " ").getOrElse("")}PSA on WTUL")
    if (spot.previousAlerts.size > 0) {
      println(s"PREVIOUS ALERTS: ${spot.previousAlerts.map(_.format(ALERT_DATE)).mkString(", ")}")
    }
  }

  val ALERT_DATE = DateTimeFormatter.ofPattern("MMMM d, yyyy")
}
