// Voice.scala --- (c) 2021 John Maraist
// Part of the WTUL Roster Generator: integrating with Google
// text-to-speech.
//
// This file is made available under the GNU GPL version 3; see the
// LICENSE file in this distribution or https://fsf.org/ for more
// details.

package org.maraist.wtulrosters
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
import org.maraist.structext.SpeakAs
import org.maraist.structext.StructText.*

object Voice {

  def listVoices = {
    val client: TextToSpeechClient = TextToSpeechClient.create()
    // Builds the text to speech list voices request
    val request: ListVoicesRequest = ListVoicesRequest.getDefaultInstance()

    // Performs the list voices request
    val response: ListVoicesResponse = client.listVoices(request)
    val voices = response.getVoicesList()

    val voiceIter = voices.iterator
    while (voiceIter.hasNext) {
      val voice = voiceIter.next
      // Display the voice's name. Example: tpc-vocoded
      println(s"Name: ${voice.getName}")

      // Display the supported language codes for this
      // voice. Example: "en-us"
      val languageCodes =
        voice.getLanguageCodesList().asByteStringList().iterator()
      while (languageCodes.hasNext) {
        val languageCode: ByteString = languageCodes.next
        println(s"- Supported Language: ${languageCode.toStringUtf8()}")
      }

      println(s"- Gender ${voice.getSsmlGender()}")

      // Display the natural sample rate hertz for this
      // voice. Example: 24000
      println(s"- Sample rate: ${voice.getNaturalSampleRateHertz()}Hz")
    }
  }

  /**
    * Demonstrates using the Text to Speech client to synthesize text or ssml.
    */
  def translateTestPlain = {
    val textToSpeechClient: TextToSpeechClient = TextToSpeechClient.create()
    // Set the text input to be synthesized
    val input: SynthesisInput =
      SynthesisInput.newBuilder()
        .setText(PsaLongTermSpots("VoteDotOrg").get.text.toPlain(80))
        .build()

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
    val out: OutputStream = new FileOutputStream("plain-test.mp3")
    out.write(audioContents.toByteArray())
    println("Audio content written to file \"plain-test.mp3\"")
  }

  /**
    * Demonstrates using the Text to Speech client to synthesize text or ssml.
    */
  def translateTestSSML = {
    val textToSpeechClient: TextToSpeechClient = TextToSpeechClient.create()
    // Set the text input to be synthesized
    val input: SynthesisInput =
      SynthesisInput.newBuilder()
        .setSsml(PsaLongTermSpots("VoteDotOrg").get.toSSML)
        .build()

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
    val out: OutputStream = new FileOutputStream("spot-test.mp3")
    out.write(audioContents.toByteArray())
    println("Audio content written to file \"spot-test.mp3\"")
  }

  /**
    * Demonstrates using the Text to Speech client to synthesize text
    * or ssml.
    */
  def translateTestSSML2 = PsaLongTermSpots("VoteDotOrg").get.writeSpotAudio

  /**
    * Demonstrates using the Text to Speech client to synthesize text
    * or ssml only when needed to update a spot.
    */
  def translateTestSSML3 = {
    PsaLongTermSpots("VoteDotOrg").get.updateSpotAudio
    PsaLongTermSpots("GiftOfLifeMarrowTwo").get.updateSpotAudio
    PsaLongTermSpots("PAWS").get.updateSpotAudio
    PsaShortTermSpots("MusicaSept2021This").get.updateSpotAudio
    println(PsaShortTermSpots("MusicaSept2021This").get.toSSML)
  }

  def translateTestSSML4 = println(PsaLongTermSpots("VoteDotOrg").get.hashCode)
}
