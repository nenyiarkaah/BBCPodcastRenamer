package services

import org.scalatest.Matchers._
import org.scalatest.{BeforeAndAfter, FlatSpec}

/**
  * Created by Nenyi on 21/03/2017.
  */
class SimpleJsonTest extends FlatSpec with BeforeAndAfter with SimpleJson with testData {

  "ConvertPodcastSettingsToJson" should "be able to convert PodcastSettings to JSON" in {
    ConvertPodcastSettingsToJson(defaultSettings) shouldBe testJSON
  }

  "ConvertJsonToPodcastSettings" should "be bale to convert JSON to PodcastSettings" in {
    val result = ConvertJsonToPodcastSettings(testJSON.toString)
    result.destination shouldEqual defaultSettings.destination
    result.source shouldBe defaultSettings.source
    result.extensions shouldBe defaultSettings.extensions
  }
}
