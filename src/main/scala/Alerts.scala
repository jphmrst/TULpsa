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
    val emailAlertable = {
      val builder = Set.newBuilder[Spot]
      for (
        spot <- PsaLongTermSpots.all;
        if spot.alert.isBefore(now) && spot.sourceContacts.length > 0
      ) do builder += spot
      builder.result
    }
    println(s"${emailAlertable.size} spot(s) alertable by email")

    val webAlertable = {
      val builder = Set.newBuilder[Spot]
      for (
        spot <- PsaLongTermSpots.all;
        if spot.alert.isBefore(now) && spot.sourceContacts.length == 0
        && spot.sourceURL.length > 0
      ) do builder += spot
      builder.result
    }
    println(s"${webAlertable.size} spot(s) alertable online")

    for (spot <- webAlertable) do {
      println("============================================================")
      println(spot.tag)
      println("Check online at")
      for (url <- spot.sourceURL) do println("- " + url)
    }

    for (spot <- emailAlertable) do {
      println("============================================================")
      var toSep = "To: "
      for (to <- spot.sourceContacts) do {
        print(toSep + to)
        toSep = ", "
      }
      println()
      println(s"Subject: ${spot.sourceName.map(_ + " ").getOrElse("")}PSA on WTUL")
      if (spot.previousAlerts.size > 0) {
        println(s"PREVIOUS ALERTS: ${spot.previousAlerts.map(_.format(ALERT_DATE)).mkString(", ")}")
      }
      println()
      println("Hi,")
      println()
      println(s"I'm writing about the ${spot.sourceName.map(_ + " ").getOrElse("")}PSA which we are currently running in a long-term rotation on WTUL.  I have a note that you are the contact for this announcement, and I'd like to make sure that it is still current.  Please have a look at the text below, and let me know about any updates or changes that we should make to it.")
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
  }

  val ALERT_DATE = DateTimeFormatter.ofPattern("MMMM d, yyyy")
}
