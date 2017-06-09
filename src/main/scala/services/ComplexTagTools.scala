package services

import java.io.File

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import domains._
import org.jaudiotagger.audio.AudioFileIO
import org.jaudiotagger.tag.{FieldKey, Tag}
import OutCome._

/**
  * Created by Nenyi on 09/04/2017.
  */
trait ComplexTagTools extends SimpleTagTools {

  def constructPodcastItems(podcastFiles: List[File]): Future[List[PodcastItem]] = {
    val futures = podcastFiles.map(constructPodcastItem(_))
    val podcastList = Future.sequence(futures)
    for {
      podcastItems <- podcastList
    } yield podcastItems.flatten
  }

  def isUnprocessedBBCPodcast(podcastItems: Future[List[PodcastItem]]) = {
    for {
      items <- podcastItems
    } yield items.filter(p => isUnprocessedBBCPodcastTag(p.tag) == true)
  }


  def isProcessedBBCPodcast(podcastItems: Future[List[PodcastItem]]) = {
    for {
      items <- podcastItems
    } yield items.filter(p => isProcessedBBCPodcastTag(p.tag) == true)
  }

  @deprecated
  def extractArtistAndTitleTagField(tag: Tag, seperator: String) = {
    val originalTitle = tag.getFirst(FieldKey.TITLE)
    val combined = originalTitle.split(seperator)
    Map("artist" -> combined.headOption.getOrElse("Unknown"), "title" -> combined.lastOption.getOrElse("Unknown"))
  }

  @deprecated
  def renameArtist(artist: String, patterns: List[RenamePattern]): String = {
    patterns match {
      case List() => artist
      case p :: ps => val newArtist = p.pattern replaceAllIn(artist, p.replacement)
        renameArtist(newArtist, ps)
    }
  }

  def TransformPodcastTags(item: PodcastItem, transformFunctions: Seq[(Tag) => Tag]): Future[PodcastItem] = Future {
    val tag = item.tag

    def consumeTransforms(tag: Tag, transforms: Seq[(Tag) => Tag]): Tag = {
      transforms match {
        case Seq() => tag
        case f +: fs => consumeTransforms(f(tag), fs)
      }
    }

    val newTag: Tag = consumeTransforms(tag, transformFunctions)
    println("Renaming tags from " + tag.getFirst(FieldKey.ARTIST) + "-" + tag.getFirst(FieldKey.TITLE) + " to " + newTag.getFirst(FieldKey.ARTIST) + "-" + newTag.getFirst(FieldKey.TITLE))
    new PodcastItem(item.podcastFile, newTag, item.fileName, item.destDir, TagsRenamed)
  }


  def renameFilenameFromTags(item: PodcastItem, fields: Seq[FieldKey], separator: String) = Future {
    val fileName = stripIllegalCharacters(fields.map(item.tag.getFirst(_)).mkString(separator))
    new PodcastItem(item.podcastFile, item.tag, fileName, item.destDir, FileRenamed)
  }

  def writeToPodcast(item: PodcastItem): PodcastItem = {
    println("Writing tags to " + item.fileName)
    val podcast = AudioFileIO.read(item.podcastFile)
    podcast.setTag(item.tag)
    podcast.commit()
    item
  }


  def getPodcastDestination(item: PodcastItem, root: String): PodcastItem = {
    val s = new SimpleFileTools
    val tag = item.tag
    val sub = tag.getFirst(FieldKey.ALBUM)
    val destination = s.createDestination(root, sub)
    new PodcastItem(item.podcastFile, item.tag, item.fileName, destination, RootDestinationMapped)
  }
}
