// Texts.scala --- (c) 2021 John Maraist
// Part of the WTUL Roster Generator
//
// This file is made available under the GNU GPL version 3; see the
// LICENSE file in this distribution or https://fsf.org/ for more
// details.

package org.maraist.wtulrosters
import org.maraist.structext.{StructText, fromString}
import org.maraist.structext.StructText.*

def am(time: String): StructText = sc(s"{time}am")
def pm(time: String): StructText = sc(s"{time}pm")

def online(txt: String): StructText = sf(str(txt))

def moreWebPhoneAnnounce(ann: String, web: String, phone: String):
    StructText = (
  fromString(ann) + fromString("available online at")
    + sf(s"$web,") + fromString("or by phone at " + phone + ".")
)

def moreWebEmailAnnounce(ann: String, web: String, email: String):
    StructText = (
  fromString(ann) + fromString("available online at")
    + sf(s"$web,") + fromString("or by email to") + sf(s"$email.")
)

def moreWebAnnounce(ann: String, web: String): StructText =
  fromString(ann) + fromString("available online at") + sf(s"$web.")

def moreEmailAnnounce(ann: String, email: String): StructText =
  fromString(ann) + fromString("available by email to") + sf(s"$email.")

def morePhoneAnnounce(ann: String, phone: String): StructText =
  fromString(s"$ann available by phone at $phone.")

def morePhoneEmailAnnounce(ann: String, phone: String, email: String):
    StructText =
  fromString(s"$ann available by phone at $phone, or by email to")
    + sf(s"$email.")

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
