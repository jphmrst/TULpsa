// Utils.scala --- (c) 2021 John Maraist
// Part of the WTUL Roster Generator
//
// This file is made available under the GNU GPL version 3; see the
// LICENSE file in this distribution or https://fsf.org/ for more
// details.

package org.maraist.wtulrosters
import scala.sys.process.*
import java.time.{LocalDate,LocalDateTime}
import java.time.format.DateTimeFormatter

/** Miscellaneous utilities. */
object Utils {

  trait Converters {

    /** Convert a [[String]] to a [[java.time.LocalDate][LocalDate]] by
      * parsing the string.
      */
    given stringToLocalDate: Conversion[String, LocalDate] = LocalDate.parse(_)

    /** Convert any [[A]] instance to an [[Option]][A] instance by tagging
      * it [[Some]].
      */
    given optionPresent[A]: Conversion[A, Option[A]] with
      def apply(a: A): Option[A] = Some(a)

    /** Convert any [[A]] instance to an [[Seq]][A] instance by making it
      * a singleton sequence.
      */
    given singletonSeq[A]: Conversion[A, Seq[A]] with
      def apply(a: A): Seq[A] = Seq(a)
  }

  /** Transforms a list by inserting `x` in between each element of the
    * argument in the result.
    */
  def insertBetween[A](x: A, xs: List[A]): List[A] = xs match {
    case Nil => Nil
    case (y :: Nil) => xs
    case (y :: z :: zs) => y :: x :: insertBetween(x, z :: zs)
  }

  /** Use `rsync` to upload the given files to the remote directory
    * specified by the environment variable `WTUL_ROSTERS_UPLOAD`.  If
    * that variable is not set, then this method does nothing.
    * Additional options to `rsync` can be specified through the
    * `WTUL_ROSTERS_RSYNC_OPTS` environment variable.
    */
  def syncRosters(files: Seq[String]): Unit = {
    import scala.util.Properties.{envOrNone, envOrElse}
    envOrNone("WTUL_ROSTERS_UPLOAD") match {
      case None => {
        Output.infoln(s"No WTUL_ROSTERS_UPLOAD; skipping file sync")
      }
      case Some(hostPath) => {
        // Make an index.html file as a manifest
        Output.info(s"Writing index file...")
        import java.io.File
        import java.io.FileWriter
        import java.io.BufferedWriter
        var bw: BufferedWriter =
          new BufferedWriter(new FileWriter(new File("index.html")))
        bw.write("""
<html>
  <head>
    <title>WTUL Current rosters</title>
  </head>
  <body>
    <h1>WTUL Current rosters</h1>
    <ul>
""")
        for (file <- files) {
          bw.write(s"      <li><a href=\"$file\">$file</a></li>\n")
        }

        bw.write("""    </ul>
  <p><i>Last generated: """)
        bw.write(LocalDateTime.now().format(
          DateTimeFormatter.ofPattern("EEEE, MMMM d, yyyy, h:mma")))
        bw.write("""</i></p>
  </body>
</html>
""")
        bw.close
        println("written")

        // Upload everything
        import scala.language.postfixOps
        Output.info(s"Uploading to ${hostPath}...")
        val options = envOrElse("WTUL_ROSTERS_RSYNC_OPTS", "")
        (s"rsync --delete-excluded --recursive $options index.html weeks.xml src/main/xml/rosterexport.xsd ${files.fold("")(_ + " " + _)} $hostPath" !)
      }
    }
  }

  def twoPlaces(d: Double): String = "%.2f".format(d)

  def fourPlaces(d: Double): String = "%.4f".format(d)
}
