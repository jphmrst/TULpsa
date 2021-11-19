// Voice.scala --- (c) 2021 John Maraist
// Part of the WTUL Roster Generator: integrating with Google
// text-to-speech.
//
// This file is made available under the GNU GPL version 3; see the
// LICENSE file in this distribution or https://fsf.org/ for more
// details.

package org.maraist.wtulrosters

import com.google.cloud.texttospeech.v1.ListVoicesRequest
import com.google.cloud.texttospeech.v1.ListVoicesResponse
import com.google.cloud.texttospeech.v1.TextToSpeechClient
import com.google.cloud.texttospeech.v1.Voice
import com.google.protobuf.ByteString

object Voice {

  def listVoices = {
    try {
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
          println(s"Supported Language: ${languageCode.toStringUtf8()}")
        }

        println(s"- Gender ${voice.getSsmlGender()}")

        // Display the natural sample rate hertz for this
        // voice. Example: 24000
        println("Sample rate: ${voice.getNaturalSampleRateHertz()}Hz")
      }
    }
  }
}
