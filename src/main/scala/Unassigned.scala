// Unassigned.scala --- (c) 2021 John Maraist
// Part of the WTUL Roster Generator
//
// This file is made available under the GNU GPL version 3; see the
// LICENSE file in this distribution or https://fsf.org/ for more
// details.

package org.maraist.wtulrosters

/** Simple singleton class used to fill in an unassigned slot in a
  * [[RosterBuilder]].
  */
class Unassigned private { }

object Unassigned {
  /** The singleton instance of the [[Unassigned]] class.
    */
  val ITEM = new Unassigned
}
