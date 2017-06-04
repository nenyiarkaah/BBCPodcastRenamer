package domains

import spray.json.DefaultJsonProtocol
/**
  * Created by Nenyi on 21/03/2017.
  */
object PodSettingsJsonProtocol extends DefaultJsonProtocol {
  implicit val podSettingsFormat = jsonFormat3(PodSettings)
}
