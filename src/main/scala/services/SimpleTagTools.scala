package services

import java.io.File

import org.jaudiotagger.audio.{AudioFile, AudioFileIO}
import org.jaudiotagger.tag.{FieldKey, Tag}
import java.util.logging.{Level, Logger}

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Failure, Success, Try}
import domains._

import scala.util.matching.Regex
import OutCome._

/**
  * Created by Nenyi on 24/03/2017.
  */
trait SimpleTagTools {

  def extractPodcastTagsFuture(file: File): Future[Option[Tag]] = Future {
    Logger.getLogger("org").setLevel(Level.OFF)
    Try(AudioFileIO.read(file).getTag)
    match {
      case Success(t) => Some(t)
      case Failure(e) => println(e)
        None
    }
  }

  def extractPodcastTagsTry(file: File): Option[Tag] = {
    Logger.getLogger("org").setLevel(Level.OFF)
    Try(AudioFileIO.read(file).getTag)
    match {
      case Success(t) => Some(t)
      case Failure(e) => println(e)
        None
    }
  }

  def extractPodcastTags(file: File): Tag = {
    Logger.getLogger("org").setLevel(Level.OFF)
    AudioFileIO.read(file).getTag
  }

  val unprocessedBBCPodcast = Map("composer" -> "BBC iPlayer", "genre" -> "Music")

  val processedBBCPodcast = Map("composer" -> "BBC iPlayer", "genre" -> "Podcast")

  def isBBCPodcastTag(tag: Tag, criteria: Map[String, String]): Boolean = {
    val composer = tag.getFirst(FieldKey.COMPOSER)
    val genre = tag.getFirst(FieldKey.GENRE)
    composer == criteria("composer") && genre == criteria("genre")
  }

  def isUnprocessedBBCPodcastTag(tag: Tag) = isBBCPodcastTag(tag, unprocessedBBCPodcast)

  def isProcessedBBCPodcastTag(tag: Tag) = isBBCPodcastTag(tag, processedBBCPodcast)

  def isUnprocessedBBCPodcast(podcastItem: PodcastItem): Boolean = {
    isUnprocessedBBCPodcastTag(podcastItem.tag)
  }

  def isProcessedBBCPodcast(podcastItem: PodcastItem): Boolean = {
    isProcessedBBCPodcastTag(podcastItem.tag)
  }

  def constructItem(file: File, t: Tag): Option[PodcastItem] = Some(new PodcastItem(file, t, file.getName, "", TagsRead))

  def constructPodcastItem(file: File): Future[Option[PodcastItem]] = Future {
    Try(extractPodcastTags(file)) match {
      case Success(t) => constructItem(file, t)
      case Failure(e) => println(e)
        None
    }
  }

  //not tested
  def extractField(tag: Tag, fieldKey: FieldKey) = {
    tag.getFirst(fieldKey)
  }

  val renameField = (tag: Tag, fieldKey: FieldKey, patterns: List[RenamePattern]) => {
    val field = tag.getFirst(fieldKey)

    def rename(field: String, patterns: List[RenamePattern]): String = {
      patterns match {
        case List() => field
        case p :: ps => val newField = p.pattern replaceAllIn(field, p.replacement)
          rename(newField, ps)
      }
    }

    tag.setField(fieldKey, rename(field, patterns))
    tag
  }

  val copyField = (tag: Tag, sourceKey: FieldKey, destinationKey: FieldKey) => {
    val keyValue = tag.getFirst(sourceKey)
    tag.setField(destinationKey, keyValue)
    tag
  }

}
