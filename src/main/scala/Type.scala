// Type.scala --- (c) 2021 John Maraist
// Part of the WTUL Roster Generator
//
// This file is made available under the GNU GPL version 3; see the
// LICENSE file in this distribution or https://fsf.org/ for more
// details.

package org.maraist.wtulrosters
import java.time.LocalDate

/** Highest-level abstraction of the method for constructing rosters.
  */
sealed trait RosterType {
  def writeFor(date: LocalDate): Unit
}

abstract class RosterTypeImpl[Builder <: RosterBuilder](
  val builderFactory: (LocalDate) => Builder
) extends RosterType {
  def writeFor(date: LocalDate) = {
    val builder = builderFactory(date)
    complete(builder, date)
    builder.result().write()
  }

  protected def complete(builder: Builder, date: LocalDate): Unit
}
