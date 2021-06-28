
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
  val indexFormatter: Int => String,
  val preamble: String
) extends LaTeXRenderable {
  if (slots.size != size) {
    throw new IllegalArgumentException(
      "Expected " + size.toString() + " spots, but array has length "
        + slots.size.toString())
  }

  def insertBetween[A](x: A, xs: List[A]): List[A] = xs match {
    case Nil => Nil
    case (y :: Nil) => xs
    case (y :: z :: zs) => y :: x :: insertBetween(x, z :: zs)
  }

  def toLaTeX(doc: LaTeXdoc): Unit = {
    val groups: List[SpotGroup] = SpotGroup.from(slots)

    for(group <- groups) {
      val blockText =
        "\\LARGE\\textsf{\\bfseries PSA \\#"
        + insertBetween(
          "\\,$\\cdot$\\,",
          List.from(
            for(i <- 0 until group.count)
            yield (group.firstIndex + i).toString()
          )
        ).fold("")(_ + _)
        + "}"

      doc ++= "\\par\n"
      doc ++= s"\\settowidth{\\nextPsaBlock}{$blockText}\n"
      doc ++= "\\addtolength{\\nextPsaBlock}{1pt}\n"
      doc ++= "\\setlength{\\nextPsaBlockWrap}{\\nextPsaBlock}\n"
      doc ++= "\\addtolength{\\nextPsaBlockWrap}{2\\psaBlockSep}\n"
      doc ++= "\\begin{minipage}{\\textwidth}\n"
      doc ++= "\\begin{wrapfigure}{l}{\\nextPsaBlockWrap}\n"
      doc ++= """\begin{tikzpicture}[every node/.style={fill,text=white,text width=\nextPsaBlock,inner sep=\psaBlockSep, outer sep=0pt}] \node {"""
      doc ++= blockText
      doc ++= "};\\end{tikzpicture}\n"
      doc ++= "\\end{wrapfigure}\n"

      doc ++= "\\textsl{Optional introduction: "
      doc ++= group.spot.introText
      doc ++= "}\n"

      doc ++= "\\\\\n"
      doc ++= group.spot.text
      doc ++= "\n\\end{minipage}\n"
    }
  }

  def write(dir: String = "./") = {
    val doc = openDoc
    toLaTeX(doc)
    closeDoc(doc)
  }

  def fileTitle: String = startDate.toString()

  def openDoc: LaTeXdoc = {
    val doc = new LaTeXdoc(fileTitle)
    doc.setClass("book")
    doc.setClassOptions("12pt")
    // doc.addPackage("rosters")
    doc.addPackage("times")
    doc.addPackage("tikz")
    doc.addPackage("color")
    doc.addPackage("geometry", "left=0.5in, right=0.5in, top=0.7in, bottom=0.7in")
    doc.addPackage("wrapfig")
    doc.addPreamble(preamble)
    doc.addPreamble("""\def\rosterDates{FILL IN dates}
""")
    doc.open()
    doc ++= commonStart
    doc
  }

  def closeDoc(doc: LaTeXdoc) = {
    doc.close()
  }
}

val commonStart: String = """
\pagestyle{fancy}
\setcounter{page}{1}
\def\rosterName{PSA roster}
"""

val commonPreamble: String = """
\newcommand{\online}[1]{\textsl{#1}}
\headheight 12pt
\headsep 1em
\newlength{\psaBlockSep}
\setlength{\psaBlockSep}{2.5mm}
\setlength\intextsep{0pt} % Squashes extra space around labels
\newlength{\nextPsaBlock}
\newlength{\nextPsaBlockWrap}

\parindent 0pt
\parskip 10pt

\def\misprintsTo{{\small Please report typos, expired spots, or other problems with PSAs to \textsl{wtul-psa@gmail.com}\,.}}

\usepackage{fancyhdr}
\def\plainheadrulewidth{0pt}
\def\footrulewidth{0.4pt}
\lhead{WTUL 91.5\textsc{fm} --- \rosterName}
\chead{}
\rhead{\rosterDates}
\lfoot[\textbf{\thepage}]{\misprintsTo}
\cfoot{}
\rfoot[\misprintsTo]{\textbf{\thepage}}

\makeatletter

%% Phone only
\def\MorePhone{\@ifnextchar[{\@MorePhoneB}{\@MorePhone}}
\def\@MorePhone#1{\@MorePhoneB[More information is]{#1}}
\def\@MorePhoneB[#1]#2{#1 available by phone; their number is #2.}

%% Web only
\def\MoreWeb{\@ifnextchar[{\@MoreWebB}{\@MoreWeb}}
\def\@MoreWeb#1{\@MoreWebB[More information is]{#1}}
\def\@MoreWebB[#1]#2{#1 available on their web site, \online{#2}.}

%% Two web sites

\def\MoreWebWeb{\@ifnextchar[{\@MoreWebWebB}{\@MoreWebWeb}}
\def\@MoreWebWeb#1#2{\@MoreWebWebB[More information is]{#1}{#2}}
\def\@MoreWebWebB[#1]#2#3{%
  #1 available on their web site, \online{#2} or \online{#3}.}

%% Email only

\def\MoreEmail{\@ifnextchar[{\@MoreEmailB}{\@MoreEmail}}
\def\@MoreEmail#1{\@MoreEmailB[More information is]{#1}}
\def\@MoreEmailB[#1]#2{#1 available by email; their address is \online{#2}.}

%% Web and phone

\def\MorePhoneWeb{\@ifnextchar[{\@MorePhoneWebB}{\@MorePhoneWeb}}
\def\MoreWebPhone{\@ifnextchar[{\@MoreWebPhoneB}{\@MoreWebPhone}}

\def\@MorePhoneWeb#1#2{\@MoreWebPhone{#2}{#1}}
\def\@MorePhoneWebB[#1]#2#3{\@MoreWebPhoneB[#1]{#3}{#2}}

\def\@MoreWebPhone#1#2{\@MoreWebPhoneB[More information is]{#1}{#2}}
\def\@MoreWebPhoneB[#1]#2#3{%
  #1 available online or by phone;  %
  their website is \online{#2}, and their number is #3.}

%% Phone and email

\def\MorePhoneEmail{\@ifnextchar[{\@MorePhoneEmailB}{\@MorePhoneEmail}}
\def\MoreEmailPhone{\@ifnextchar[{\@MoreEmailPhoneB}{\@MoreEmailPhone}}

\def\@MorePhoneEmail#1#2{\@MoreEmailPhone{#2}{#1}}
\def\@MoreEmailPhone#1#2{More information is available by email or phone;  their email address is \online{#1}, and their number is #2.}
\def\@MorePhoneEmailB[#1]#2#3{\@MoreEmailPhoneB[#1]{#3}{#2}}
\def\@MoreEmailPhoneB[#1]#2#3{#1 available by email or phone;  their email address is \online{#2}, and their number is #3.}

%% Web and email

\def\MoreWebEmail{\@ifnextchar[{\@MoreWebEmailB}{\@MoreWebEmail}}
\def\MoreEmailWeb{\@ifnextchar[{\@MoreEmailWebB}{\@MoreEmailWeb}}

\def\@MoreEmailWeb#1#2{\@MoreWebEmail{#2}{#1}}
\def\@MoreEmailWebB[#1]#2#3{\@MoreWebEmailB[#1]{#3}{#2}}

\def\@MoreWebEmail#1#2{\@MoreWebEmailB[More information is]{#1}{#2}}
\def\@MoreWebEmailB[#1]#2#3{%
  #1 available online;  %
  their web site is \online{#2}, and their email address is \online{#3}.}


%% Web, phone, email

\def\MoreWebPhoneEmail{%
  \@ifnextchar[{\@MoreWebPhoneEmailB}{\@MoreWebPhoneEmail}}

\def\MoreWebEmailPhone{%
  \@ifnextchar[{\@MoreWebEmailPhoneB}{\@MoreWebEmailPhone}}
\def\@MoreWebEmailPhone#1#2#3{\@MoreWebPhoneEmail{#1}{#3}{#2}}
\def\@MoreWebEmailPhoneB[#1]#2#3#4{\@MoreWebPhoneEmail[#1]{#2}{#4}{#3}}

\def\MoreEmailWebPhone{%
  \@ifnextchar[{\@MoreEmailWebPhoneB}{\@MoreEmailWebPhone}}
\def\@MoreEmailWebPhone#1#2#3{\@MoreWebPhoneEmail{#2}{#3}{#1}}
\def\@MoreEmailWebPhoneB[#1]#2#3#4{\@MoreWebPhoneEmail[#1]{#3}{#4}{#2}}

\def\MoreEmailPhoneWeb{%
  \@ifnextchar[{\@MoreEmailPhoneWebB}{\@MoreEmailPhoneWeb}}
\def\@MoreEmailPhoneWeb#1#2#3{\@MoreWebPhoneEmail{#3}{#2}{#1}}
\def\@MoreEmailPhoneWebB[#1]#2#3#4{\@MoreWebPhoneEmail[#1]{#4}{#3}{#2}}

\def\MorePhoneEmailWeb{%
  \@ifnextchar[{\@MorePhoneEmailWebB}{\@MorePhoneEmailWeb}}
\def\@MorePhoneEmailWeb#1#2#3{\@MoreWebPhoneEmail{#3}{#1}{#2}}
\def\@MorePhoneEmailWebB[#1]#2#3#4{\@MoreWebPhoneEmail[#1]{#4}{#2}{#3}}

\def\MorePhoneWebEmail{%
  \@ifnextchar[{\@MorePhoneWebEmailB}{\@MorePhoneWebEmail}}
\def\@MorePhoneWebEmail#1#2#3{\@MoreWebPhoneEmail{#2}{#1}{#3}}
\def\@MorePhoneWebEmailB[#1]#2#3#4{\@MoreWebPhoneEmail[#1]{#3}{#2}{#4}}

\def\@MoreWebPhoneEmail#1#2#3{%
  \@MoreWebPhoneEmailB[More information is]{#1}{#2}{#3}}
\def\@MoreWebPhoneEmailB[#1]#2#3#4{%
  #1 available online or by phone.  Their website is \online{#2},
  and their number is #3.}
% their phone number is #3, and their email address is \online{#4}.}

\makeatother
"""

class PsaRoster(startDate: LocalDate, slots: Array[Spot])
    extends Roster(startDate, slots, 78,
      "WTUL 91.5\\textsc{fm} --- PSA roster",
      "PSA \\#",
      "Please report typos, expired spots, or other problems with PSAs to \\textsl{wtul-psa@gmail.com}\\,.",
      (x: Int) => (1 + x).toString(),
      commonPreamble)


case class SpotGroup(
  val spot: Spot, val firstIndex: Int, val count: Int = 1) {
  def another: SpotGroup = SpotGroup(spot, firstIndex, 1 + count)
}

object SpotGroup {
  def from(spots: Array[Spot]): List[SpotGroup] =
    List.from(spots) match {
      case first :: rest => groupSpots(SpotGroup(first, 1), 2, rest)
      case Nil => Nil
    }

  def groupSpots(
    thisGroup: SpotGroup, firstIdx: Int, spots: List[Spot]):
      List[SpotGroup] = spots match {
    case firstSpot :: nextSpots => {
      val nextIdx = 1 + firstIdx
      if thisGroup.spot == firstSpot
      then groupSpots(thisGroup.another, nextIdx, nextSpots)
      else thisGroup :: groupSpots(
        SpotGroup(firstSpot, firstIdx),
        nextIdx,
        nextSpots)
    }
    case Nil => thisGroup :: Nil
  }
}
