
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
