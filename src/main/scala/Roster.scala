
package org.maraist.wtulrosters
import java.time.LocalDate
import org.maraist.latex.{LaTeXRenderable, LaTeXdoc}

abstract class Roster(
  val startDate: LocalDate,
  val slots: Array[Spot],
  val size: Int,
  val title: String,
  val groupLead: String,
  val footer: String,
  val indexFormatter: Int => String
) extends LaTeXRenderable {
  if (slots.size != size) {
    throw new IllegalArgumentException(
      "Expected " + size.toString() + " spots, but array has length "
        + slots.size.toString())
  }

  def toLaTeX(doc: LaTeXdoc): Unit = {
    ???
  }
}

class PsaRoster(startDate: LocalDate, slots: Array[Spot])
    extends Roster(startDate, slots, 78,
      "WTUL 91.5\\textsc{fm} --- PSA roster",
      "PSA \\#",
      "Please report typos, expired spots, or other problems with PSAs to \\textsl{wtul-psa@gmail.com}\\,.",
      (x: Int) => (1 + x).toString())
