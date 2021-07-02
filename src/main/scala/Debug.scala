
package org.maraist.wtulrosters

object Output {
  inline val level = 1

  inline val showInfo = level > 0
  inline val showSome = level > 1
  inline val showMore = level > 2
  inline val showFull = level > 3

  inline def info(msg: String): Unit = if showInfo then print(msg)
  inline def infoln(msg: String): Unit = if showInfo then println(msg)

  inline def some(msg: String): Unit = if showSome then print(msg)
  inline def someln(msg: String): Unit = if showSome then println(msg)

  inline def more(msg: String): Unit = if showMore then print(msg)
  inline def moreln(msg: String): Unit = if showMore then println(msg)

  inline def full(msg: String): Unit = if showFull then print(msg)
  inline def fullln(msg: String): Unit = if showFull then println(msg)
}
