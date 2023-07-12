// Spot.scala --- (c) 2021 John Maraist
// Part of the WTUL Roster Generator
//
// This file is made available under the GNU GPL version 3; see the
// LICENSE file in this distribution or https://fsf.org/ for more
// details.

package org.maraist.wtulrosters
import java.time.LocalDate
import java.util.Scanner
import java.io.{OutputStream, FileOutputStream, File, FileWriter}
import com.google.cloud.texttospeech.v1.ListVoicesRequest
import com.google.cloud.texttospeech.v1.ListVoicesResponse
import com.google.cloud.texttospeech.v1.TextToSpeechClient
import com.google.cloud.texttospeech.v1.{
  AudioConfig, AudioEncoding, SsmlVoiceGender, SynthesisInput,
  VoiceSelectionParams, SynthesizeSpeechResponse}
import com.google.cloud.texttospeech.v1.Voice
import com.google.protobuf.ByteString
import com.google.protobuf.ByteString
import org.maraist.structext.{StructText, ProsodyPitch}
import org.maraist.structext.StructText.*
import org.maraist.latex.LaTeXdoc

/** Class representing one announcement.
  *
  * @param tag A short [[String]] naming this announcement.
  * @param group The [[Group]] which includes this spot.
  * @param text The body of the announcement to be read.
  * @param copresent If present, a string representing the group or
  * person which should be credited as a co-presenter of the
  * announcement.
  * @param start Start date of this announcement.
  * @param alert Date when we should see a reminder for updating this
  * announcement.
  * @param end If present, gives the end date of this announcement
  * (the last day on which it should be aired).
  * @param sourceContacts A list of names or email addresses of the
  * contact people at the organization associated with this event.
  * @param sourceURL A list of URLs associated with the sourcing of
  * this announcement.
  * @param sourceNote If present, a note about the sourcing of this
  * spot.
  * @param groupGainMultiplier A factor to be applied to the gain
  * taken from the group when applied to this spot.
  * @param variantGroup An identification number to be shared among
  * announcements of the same source.
  * @param boost An upwards compression factor to be applied to this
  * spot in any [[Assortment]].
  * @param bank The [[SpotBank]] in which this spot is contained.
  */
class Spot(
  val tag: String,
  val group: Group,
  val text: StructText,
  val copresent: Option[String] = None,
  val start: LocalDate,
  val alert: LocalDate,
  val end: Option[LocalDate] = None,
  val sourceContacts: Seq[String] = Seq(),
  val sourceURL: Seq[String] = Seq(),
  val sourceNote: Option[String] = None,
  val orgName: Option[String] = None,
  val groupGainMultiplier: Double = 1.0,
  val variantGroup: Int = Spot.nextVariantGroup,
  val boost: Double = 0.0,
  val previousAlerts: Seq[LocalDate] = Seq()
)(using addSpot: (Spot) => Unit) {

  override def toString(): String = tag

  import Spot.{getWeekOfCentury, EPSILON}

  Output.fullln(s"Spot $tag, ${group.tag}, $start${end.map(" to " + _.toString()).getOrElse("")}, boost $boost")

  // -----------------------------------------------------------------
  // Instance creation.

  // Correctness checks --- make sure each spot has a unique tag.
  if boost >= 1.0 || boost < 0.0 then
    throw new IllegalArgumentException
      ("boost must be at least 0.0 and below 1.0")

  // Perform other checks, and then (err or) register this instance in
  // the various Spot tables.
  addSpot(this)

  // End of instance creation.
  // -----------------------------------------------------------------

  def validOn(date: LocalDate): Boolean =
    start.compareTo(date) <= 0
      && end.map(date.compareTo(_) <= 0).getOrElse(true)

  override val hashCode: Int = tag.hashCode + start.hashCode + group.hashCode

  /** Calculate a small, unique, persistent "wiggle" for the period of
    * every spot, so that it has at least a slightly unique pattern.
    */
  private[wtulrosters]
  val periodWiggle: Double = Math.sin(hashCode.toDouble) / 4.0

  /** The period (in weeks) of the rise and fall of this spot's priority
    * ranking.
    */
  val period: Double = group.period + periodWiggle

  /** Calculate the spot's priority on a given date.
    */
  def priority(date: LocalDate): Double = {
    val curvePoint =
      Math.sin(date.getWeekOfCentury().toDouble / period * 2.0 * Math.PI)
    val inUnit = curvePoint / (2.0 - 2 * EPSILON) + 0.5 + EPSILON
    val withSpotBoost = (inUnit * (1 - boost)) + boost
    val withGroupBoost = (withSpotBoost * (1 - group.boost)) + group.boost
    withGroupBoost
  }

  /** Assemble the introductory text (excluding the "optional" or
    * "required" prefix) for this spot.
    */
  def introText: String = {
    "This public service announcement is brought to you by "
    + copresent.map(_ + " and ").getOrElse("")
    + "WTUL New Orleans."
  }

  def toSSML: String = {
    val hash = hashCode()

    val maleFirst: Boolean = (hash % 2) == 0
    val firstVoice = if maleFirst then "male" else "female"
    val secondVoice = if maleFirst then "female" else "male"

    val variant = 1 + (hash % 8)

    val firstPitch = (hash % 5) match {
      case 1 => ProsodyPitch.Medium
      case 2 => ProsodyPitch.Medium
      case _ => ProsodyPitch.Low
    }

    val secondPitch = (hash % 7) match {
      case 0 => ProsodyPitch.Medium
      case 1 => ProsodyPitch.Medium
      case 2 => ProsodyPitch.Medium
      case _ => ProsodyPitch.Low
    }

    val sb = new StringBuilder
    sb ++= "<speak>\n"
    sb ++= s"  <voice languages=\"en-US\" gender=\"$firstVoice\" variant=\"$variant\">\n"
    sb ++= s"    <prosody rate=\"95%\" pitch=\"${firstPitch.pitch}\">\n"
    sb ++= s"      <s>${introText.replace(
      "WTUL",
      "<say-as interpret-as=\"characters\">WTUL</say-as>")}</s>\n"
    sb ++= "    </prosody>\n"
    sb ++= "  </voice>\n"
    sb ++= s"  <voice languages=\"en-US\" gender=\"$secondVoice\">\n"
    sb ++= s"    <prosody rate=\"95%\" pitch=\"${secondPitch.pitch}\">\n"
    sb ++= s"      ${text.toSSML}\n"
    sb ++= "    </prosody>\n"
    sb ++= "  </voice>\n"
    sb ++= "</speak>\n"
    sb.result
  }

  def updateSpotAudio: Unit = {
    val mp3File = new File(Spot.cacheDir, s"${tag}.mp3")
    val hashFile = new File(Spot.cacheDir, s"${tag}.txt")

    val sameHash: Boolean = if hashFile.isFile then {
      val scanner: Scanner = new Scanner(hashFile)
      val savedHash = scanner.nextInt()
      (savedHash == hashCode) && mp3File.exists
    } else false

    if !sameHash then writeSpotAudio
  }

  def writeSpotAudio = {
    val mp3File = new File(Spot.cacheDir, s"${tag}.mp3")
    print(s"Writing audio for spot \"${mp3File.toString}\"...")
    Spot.cacheDir.mkdirs
    val textToSpeechClient: TextToSpeechClient = TextToSpeechClient.create()

    // Create and save the SSML of the spot.
    val ssml = toSSML
    val ssmlWriter: FileWriter =
      new FileWriter(new File(Spot.cacheDir, s"${tag}.ssml"))
    ssmlWriter.write(ssml)
    ssmlWriter.close

    // Save the spot hash code.
    val hashWriter: FileWriter =
      new FileWriter(new File(Spot.cacheDir, s"${tag}.txt"))
    hashWriter.write(s"${hashCode}")
    hashWriter.close

    // Set the text input to be synthesized
    val input: SynthesisInput =
      SynthesisInput.newBuilder().setSsml(ssml).build()

    // Build the voice request
    val voice: VoiceSelectionParams =
        VoiceSelectionParams.newBuilder()
            .setLanguageCode("en-US") // languageCode = "en_us"
            .setSsmlGender(SsmlVoiceGender.FEMALE)
            .build()

    // Select the type of audio file you want returned
    val audioConfig: AudioConfig =
        AudioConfig.newBuilder()
            .setAudioEncoding(AudioEncoding.MP3) // MP3 audio.
            .build()

    // Perform the text-to-speech request
    val response: SynthesizeSpeechResponse =
        textToSpeechClient.synthesizeSpeech(input, voice, audioConfig)

    // Get the audio contents from the response
    val audioContents: ByteString = response.getAudioContent()

    // Write the response to the output file.
    val out: OutputStream = new FileOutputStream(mp3File)
    out.write(audioContents.toByteArray())
    println("done")
  }

  def addInventoryMetaItems(doc: LaTeXdoc): Unit = {
    doc ++= s"\\item Group: ${group.title}"
    doc ++= s"\\item Added $start\n"
    doc ++= s"\\item Due for check $alert\n"

    sourceURL.size match {
      case 0 => { }
      case 1 => sourceURL.map((url) => {
        doc ++= s"\\item Source URL: \\textsf{$url}\n" })
      case n => {
        doc ++= "\\item Source URLs\n"
        doc ++= "\\begin{compactitem}\n"
        sourceURL.map((url) => { doc ++= s"\\item\\textsf{$url}\n" })
        doc ++= "\\end{compactitem}\n"
      }
    }

    sourceContacts.size match {
      case 0 => { }
      case 1 => sourceContacts.map((who) => {
        doc ++= s"\\item Source: \\textsf{${
          who.replace("<", "$<$").replace(">", "$>$").replace("_", "\\_")
        }}\n"
      })
      case n => {
        doc ++= "\\item Sources\n"
        doc ++= "\\begin{compactitem}\n"
        sourceContacts.map((who) => {
          doc ++= s"\\item\\textsf{${
            who.replace("<", "$<$").replace(">", "$>$").replace("_", "\\_")
          }}\n" })
        doc ++= "\\end{compactitem}\n"
      }
    }

    sourceNote.map((sourceNote) => {
      doc ++= s"\\item Source note: ${sourceNote}"
    })

    orgName.map((org) => { doc ++= s"\\item Organization name: $org" })
    if (groupGainMultiplier != 1.0) {
      doc ++= s"\\item Group gain multiplier: $groupGainMultiplier"
    }

    copresent.map((co) => {
      doc ++= s"\\item Co-presenter in PSA introduction: ${co}"
    })

    if (groupGainMultiplier != 1.0) {
      doc ++= s"\\item Nonstandard group gain multiplier: $groupGainMultiplier"
    }
    if (boost != 0.0) {
      doc ++= s"\\item Nonzero boost: $boost"
    }
  }
}

/** Utilities for [[Spot]]s. */
object Spot {

  val cacheDir = File("audio")

  /** Calculate the week number of a date with its century, under the
    * simplifying assumptions that each year has exactly 52 weeks, and
    * January 1 always starts a new week.
    */
  extension (localDate: LocalDate)
      def getWeekOfCentury(): Int =
        (localDate.getYear() % 100) * 52 + localDate.getDayOfYear() / 7

  /** A very small number, used to return non-zero priorities.
    */
  val EPSILON: Double = 0.0000001

  private var nextVariantCounter: Int = 1

  /** Returns the next value available for identifying spots which
    * arise from the multi-announcement specification.
    */
  def nextVariantGroup: Int = {
    val result = nextVariantCounter
    nextVariantCounter = nextVariantCounter + 1
    result
  }
}
