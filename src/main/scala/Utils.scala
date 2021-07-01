// Utils.scala --- (c) 2021 John Maraist
// Part of the WTUL Roster Generator
//
// This file is made available under the GNU GPL version 3; see the
// LICENSE file in this distribution or https://fsf.org/ for more
// details.

package org.maraist.wtulrosters

/** Miscellaneous utilities. */
object Utils {

  /** Transforms a list by inserting `x` in between each element of the
    * argument in the result.
    */
  def insertBetween[A](x: A, xs: List[A]): List[A] = xs match {
    case Nil => Nil
    case (y :: Nil) => xs
    case (y :: z :: zs) => y :: x :: insertBetween(x, z :: zs)
  }
}
