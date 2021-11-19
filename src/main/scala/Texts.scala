// Texts.scala --- (c) 2021 John Maraist
// Part of the WTUL Roster Generator
//
// This file is made available under the GNU GPL version 3; see the
// LICENSE file in this distribution or https://fsf.org/ for more
// details.

package org.maraist.wtulrosters
import org.maraist.structext.{StructText, fromString}
import org.maraist.structext.StructText.*

val am: StructText = sc("am")
val pm: StructText = sc("pm")
val period: StructText = str(".")
val comma: StructText = str(",")

def online(txt: String): StructText = sl(str(txt))

def moreWebPhoneAnnounce(ann: String, web: String, phone: String):
    StructText = (
  fromString(ann) + fromString("available online at")
    + (online(web) > str(",")) + fromString("or by phone at " + phone + ".")
)

def moreWebEmailAnnounce(ann: String, web: String, email: String):
    StructText = (
  fromString(ann) + fromString("available online at")
    + (online(web) > str(","))
    + fromString("or by email to") + (online(email) > str("."))
)

def moreWebAnnounce(ann: String, web: String): StructText =
  fromString(ann) + fromString("available online at") + online(web) > str(".")

def moreEmailAnnounce(ann: String, email: String): StructText =
  fromString(ann) + fromString("available by email to") + online(email)
    > str(".")

def morePhoneAnnounce(ann: String, phone: String): StructText =
  fromString(s"$ann available by phone at $phone.")

def morePhoneEmailAnnounce(ann: String, phone: String, email: String):
    StructText =
  fromString(s"$ann available by phone at $phone, or by email to")
    + online(email) > str(".")

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
