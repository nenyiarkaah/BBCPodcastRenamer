package domains

import java.io.File

import domains.OutCome.OutCome
import org.jaudiotagger.tag.Tag

import scala.util.matching.Regex
/**
  * Created by Nenyi on 21/03/2017.
  */
case class PodcastItem(podcastFile: File, tag: Tag, fileName: String, destDir: String, outCome: OutCome)
case class BBCTags (artist: String, title: String, album: String, albumArtist: String,  genre: String)
case class PodSettings(source: String, destination: String, extensions: Array[String])

case class RenamePattern(pattern: Regex, replacement: String)

object OutCome extends Enumeration {
  type OutCome = Value
  val TagsRead, TagsRenamed, RootDestinationMapped, DestinationMapped, FileRenamed, ReTagged, MoveSuccessful, MoveUnSuccessful = Value
}