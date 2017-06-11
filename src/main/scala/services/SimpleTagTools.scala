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

  def isUnprocessedBBCPodcastTag(tag: Tag): Boolean = {
    val unprocessedBBCPodcast = Map("composer" -> "BBC iPlayer", "genre" -> "Podcast")
    val composer = tag.getFirst(FieldKey.COMPOSER)
    val genre = tag.getFirst(FieldKey.GENRE)
    composer == unprocessedBBCPodcast("composer") && genre != unprocessedBBCPodcast("genre")
  }

  def isProcessedBBCPodcastTag(tag: Tag): Boolean = {
    val processedBBCPodcast = Map("composer" -> "BBC iPlayer", "genre" -> "Podcast")
    val composer = tag.getFirst(FieldKey.COMPOSER)
    val genre = tag.getFirst(FieldKey.GENRE)
    composer == processedBBCPodcast("composer") && genre == processedBBCPodcast("genre")
  }

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

  val removeFieldFromField = (tag: Tag, referenceKey: FieldKey, destinationKey: FieldKey) => {
    val referenceValue = tag.getFirst(referenceKey)
    val destinationValue = tag.getFirst(destinationKey)
    val newDestinationValue = removeLeadingPunctuation(destinationValue.replaceFirst(referenceValue, ""))
    tag.setField(destinationKey, newDestinationValue)
    tag
  }

  val splitField = (tag: Tag, key: FieldKey, seperator: String, index: Int) => {
    val value = tag.getFirst(key)
    val newValue = value.split(seperator)(index)
    tag.setField(key, newValue)
    tag
  }

  def removeLeadingPunctuation(str: String): String = {
    str.split("").toList match {
      case List() => ""
      case head :: tail =>
        isAlphaNumeric(head) match {
          case true => head :: tail mkString ("")
          case false => removeLeadingPunctuation(tail.mkString(""))
        }
    }

  }

  val alphaNumeric = (('a' to 'z') ++ ('A' to 'Z') ++ ('0' to '9')).toSet

  def isAlphaNumeric(s: String) = s.forall(alphaNumeric.contains(_))

  def stripIllegalCharacters(fileName: String) = fileName.replace("\\", "-").replace("/", "-").replace("!", "")
}
