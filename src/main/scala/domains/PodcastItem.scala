package domains

import java.io.File

import domains.OutCome.OutCome
import org.jaudiotagger.tag.Tag

import scala.util.matching.Regex
/**
  * Created by Nenyi on 21/03/2017.
  */
case class PodcastItem(podcastFile: File, tag: Tag, fileName: String, destDir: String, outCome: OutCome)