
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
