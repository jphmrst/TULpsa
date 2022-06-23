// Texts.scala --- (c) 2021 John Maraist
// Part of the WTUL Roster Generator
//
// This file is made available under the GNU GPL version 3; see the
// LICENSE file in this distribution or https://fsf.org/ for more
// details.

package org.maraist.wtulrosters
import org.maraist.structext.{
  StructText, ProsodyRate, PauseWeight, SpeakAs, fromString}
import org.maraist.structext.StructText.*

val am: StructText = sc("am")
val pm: StructText = sc("pm")
val period: StructText = str(".")
val comma: StructText = str(",")

def online(txt: String): StructText =
  prosody(sl(str(txt)), rate=ProsodyRate.Percent(75))

def moreWebPhoneAnnounce(ann: String, web: String, phone: String):
    StructText = (
  fromString(ann) + fromString("available online at") > pause(PauseWeight.Medium)
    + online(web) > str(",") > pause(PauseWeight.Medium) +
    fromString("or by phone at") + speak(phone, SpeakAs.Telephone) > period
)

def moreWebEmailAnnounce(ann: String, web: String, email: String):
    StructText = (
  fromString(ann) + fromString("available online at") > pause(PauseWeight.Medium)
    + online(web) > str(",") > pause(PauseWeight.Medium)
    + fromString("or by email to") > pause(PauseWeight.Medium) +
    online(email) > str(".")
)

def moreWebPhoneEmailAnnounce(
  ann: String, web: String, phone: String, email: String):
    StructText = (
  fromString(ann) + fromString("available online at") > pause(PauseWeight.Medium)
    + online(web) > str(",") > pause(PauseWeight.Medium)
    + fromString("by phone at") > pause(PauseWeight.Medium) +
    str(phone + ",") > pause(PauseWeight.Medium)
    + fromString("or by email to") > pause(PauseWeight.Medium) +
    online(email) > str(".")
)

def moreWebAnnounce(ann: String, web: String): StructText =
  fromString(ann) + fromString("available online at") >
    pause(PauseWeight.Medium) + online(web) > str(".")

def moreEmailAnnounce(ann: String, email: String): StructText =
  fromString(ann) + fromString("available by email to") >
    pause(PauseWeight.Medium) + online(email) > str(".")

def morePhoneAnnounce(ann: String, phone: String): StructText =
  fromString(s"$ann available by phone at") >
    pause(PauseWeight.Medium) + speak(phone, SpeakAs.Telephone) > period

def morePhoneEmailAnnounce(ann: String, phone: String, email: String):
    StructText =
  fromString(s"$ann available by phone at") >
    pause(PauseWeight.Medium) + speak(phone, SpeakAs.Telephone) >
    pause(PauseWeight.Medium) > comma +
    str("or by email to") > pause(PauseWeight.Medium) + online(email) > str(".")

def moreWebPhoneEmail(web: String, phone: String, email: String): StructText =
  moreWebPhoneEmailAnnounce("More information is", web, phone, email)

def moreWebPhone(web: String, phone: String): StructText =
  moreWebPhoneAnnounce("More information is", web: String, phone: String)

def moreWebEmail(web: String, email: String): StructText =
  moreWebEmailAnnounce("More information is", web: String, email: String)

def moreWeb(web: String): StructText =
  moreWebAnnounce("More information is", web)

def moreEmail(email: String): StructText =
  moreEmailAnnounce("More information is", email)

def morePhone(phone: String): StructText =
  morePhoneAnnounce("More information is", phone)

def morePhoneEmail(phone: String, email: String): StructText =
  morePhoneEmailAnnounce("More information is", phone, email)
