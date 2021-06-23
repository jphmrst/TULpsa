
package org.maraist.wtulrosters
import java.time.LocalDate

case class Roster(
  val startDate: LocalDate,
  val slots: Array[Spot]
) {

}
