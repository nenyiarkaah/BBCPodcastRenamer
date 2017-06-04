package services

import domains._
import PodSettingsJsonProtocol._
import spray.json._

/**
  * Created by Nenyi on 21/03/2017.
  */
trait SimpleJson {

  def ConvertPodcastSettingsToJson(content: PodSettings): JsValue = {
    content.toJson
  }

  def ConvertJsonToPodcastSettings(content: String): PodSettings = {
    content.parseJson.convertTo[PodSettings]
  }
}
