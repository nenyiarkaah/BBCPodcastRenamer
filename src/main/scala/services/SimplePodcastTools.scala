package services

import java.io.File

import domains._
import java.util.Calendar

import org.apache.commons.io.FilenameUtils
import org.jaudiotagger.tag.{FieldKey, Tag}

import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.util.{Failure, Success, Try}
import scala.util.matching.Regex
import OutCome._


/**
  * Created by Nenyi on 21/03/2017.
  */
class SimplePodcastTools extends SimpleFileTools with ComplexTagTools {

  //todo
  def processBBCPodcasts = {
    val now = Calendar.getInstance().getTime()
    println(now)
    println(settingsPath)
    Try(openAndReadSettings) match {
      case Success(settings) => process(settings)
      case Failure(exception) => println(exception.getMessage)
    }
  }

  def process(settings: PodSettings) = {
    doesDirectoryExist(settings.source, "Source directory does not exist")
    val podcastItems = getPodcasts(getFiles(settings))
    val processed = isProcessedBBCPodcast(podcastItems)
    val unProcessed = isUnprocessedBBCPodcast(podcastItems)

    val renameArtistPattern = List(RenamePattern(new Regex(": (.*)"), ""), RenamePattern(new Regex("\\A"), "("), RenamePattern(new Regex("\\z"), ")"))
    val renameTitlePattern = List(RenamePattern(new Regex("(.*): "), ""))
    val renameAlbumPattern = List(RenamePattern(new Regex("\\A"), "(BBC Radio 1)-"))
    val renameGenrePattern = List(RenamePattern(new Regex("[^\\n]+"), "Podcast"))


    val unProcessedTransforms: Seq[(Tag) => Tag] = Seq(copyField(_, FieldKey.TITLE, FieldKey.ARTIST),
      renameField(_, FieldKey.ARTIST, renameArtistPattern), copyField(_, FieldKey.ARTIST, FieldKey.ORIGINAL_ARTIST),
      renameField(_, FieldKey.TITLE, renameTitlePattern), renameField(_, FieldKey.ALBUM, renameAlbumPattern),
      copyField(_, FieldKey.ALBUM, FieldKey.ALBUM_ARTIST), renameField(_, FieldKey.GENRE, renameGenrePattern))

    val podcastFutures = for {
      processedPodcasts <- processed flatMap { p =>
        Future.sequence(p.map(renameFilenameFromTags(_, Seq(FieldKey.ARTIST, FieldKey.TITLE), "-")))
      }

      transformedUnProcessedPodcasts <- unProcessed flatMap { u =>
        Future.sequence(u.map(TransformPodcastTags(_, unProcessedTransforms)))
      }

      renamedPodcasts <- Future.sequence(transformedUnProcessedPodcasts map {
        renameFilenameFromTags(_, Seq(FieldKey.ARTIST, FieldKey.TITLE), "-")
      })

      complete = renamedPodcasts ::: processedPodcasts

    } yield complete map { c => mapPodcastDestination(getPodcastDestination(c, settings.destination)) }

    val podcasts = Await.result(podcastFutures, Duration.Inf)

    val renameAgainstExistingPodcasts = renameExistingPodcasts(podcasts, settings.extensions)

    val updatedPodcasts = renameAgainstExistingPodcasts map {
      writeToPodcast(_)
    }

    val renamingPodcastFiles = updatedPodcasts map {
      renameFile(_)
    }
    renamingPodcastFiles map {
      movePodcast(_)
    }
  }

  def getPodcasts(files: List[File]) = {
    constructPodcastItems(files)
  }

  def mapPodcastDestination(item: PodcastItem): PodcastItem = {
    println("Mapping destination folder for  " + item.fileName + " to " + item.destDir)
    val destination = item.destDir
    val size = item.podcastFile.length
    checkAndCreateParentDirectory(destination)
    val subDirectories = getListOfSubDirectories(destination)
    val validSubs = validSubDirectories(subDirectories, size, 4140)
    val validDestination = matchValidDir(subDirectories, validSubs, destination)

    new PodcastItem(item.podcastFile, item.tag, item.fileName, validDestination.getAbsolutePath, DestinationMapped)
  }

  def renameFile(item: PodcastItem): PodcastItem = {
    val file = item.podcastFile
    val extension = FilenameUtils.getExtension(file.getName)
    val name = FilenameUtils.removeExtension(item.fileName)
    val fileName = file.getParent + getSeparator + name + "." + extension
    val renamedFile = new File(fileName)
    println("renaming file from " + file.getName + " to " + renamedFile)

    def result = Try {
      file.renameTo(renamedFile)
    }

    result match {
      case Success(r) => new PodcastItem(renamedFile, item.tag, renamedFile.getName, item.destDir, FileRenamed)
      case Failure(f) => throw f
    }
  }

  def retagIfPodcastExist(item: PodcastItem, tags: List[Tag]): PodcastItem = {
    val title = item.tag.getFirst(FieldKey.TITLE)
    doesPodcastExist(title, tags) match {
      case 0 => item
      case x => {
        val tag = item.tag
        val artist: String = tag.getFirst(FieldKey.ARTIST)
        val title = tag.getFirst(FieldKey.TITLE) + " " + x
        val album = tag.getFirst(FieldKey.ALBUM)
        val genre = tag.getFirst(FieldKey.GENRE)
        val fileName = artist + "-" + stripIllegalCharacters(title)
        println("Existing Podcasts of the same name have been found. Renaming to " + fileName)
        new PodcastItem(item.podcastFile, renamePodcastTags(tag, new BBCTags(artist, title, album, album, genre)), fileName, item.destDir, ReTagged)
      }
    }
  }

  def doesPodcastExist(s: String, tags: List[Tag]): Int = {
    val occurrence = tags.filter(t => t.getFirst(FieldKey.TITLE).contains(s)).size
    occurrence
  }

  def renamePodcastTags(tag: Tag, bbcTag: BBCTags): Tag = {
    println("Renaming tags from " + tag.getFirst(FieldKey.ARTIST) + "-" +
      tag.getFirst(FieldKey.TITLE) + " to " + bbcTag.artist + "-" + bbcTag.title)
    tag.setField(FieldKey.ARTIST, bbcTag.artist)
    tag.setField(FieldKey.ORIGINAL_ARTIST, bbcTag.artist)
    tag.setField(FieldKey.TITLE, bbcTag.title)
    tag.setField(FieldKey.ALBUM, bbcTag.album)
    tag.setField(FieldKey.ALBUM_ARTIST, bbcTag.albumArtist)
    tag.setField(FieldKey.GENRE, bbcTag.genre)
    tag
  }

  def renameExistingPodcasts(items: List[PodcastItem], extensions: Array[String]) = {
    val albums = items.map(_.destDir).distinct
    val existingPodcasts = albums.map(a => getRecursiveListOfFilesByFutures(new File(a), extensions))

    val existingTagFutures = for {
     podcasts <- Future.sequence(existingPodcasts).map(_.flatten)
    } yield podcasts map { t => extractPodcastTags(t) }

    val existingTags = Await.result(existingTagFutures, Duration.Inf)

    traverseExistingAndNewPodcasts(items, existingTags)
  }

  def traverseExistingAndNewPodcasts(items: List[PodcastItem], previousTags: List[Tag]) = {

    def consume(items: List[PodcastItem], previousTags: List[Tag],
                itemBuilder: List[PodcastItem] = List[PodcastItem]()): List[PodcastItem] = {
      items match {
        case List() => itemBuilder
        case head :: tail =>
          val album = head.tag.getFirst(FieldKey.ALBUM)
          val matchingPreviousTags = previousTags.filter(p => {
            p.getFirst(FieldKey.ALBUM) == album
          })
          val newHead = retagIfPodcastExist(head,
            matchingPreviousTags)
          consume(tail, newHead.tag :: previousTags, newHead :: itemBuilder)
      }
    }

    consume(items, previousTags)
  }

  def movePodcast(item: PodcastItem) = {
    val source = item.podcastFile.toString
    val destination = item.destDir
    val file = destination + getSeparator + item.fileName
    println("moving " + item.fileName + " to " + item.destDir)
    move(source, destination) match {
      case Success(d) => new PodcastItem(new File(file), item.tag, item.fileName, destination, MoveSuccessful)
      case Failure(ex) =>
        println(ex)
        new PodcastItem(new File(file), item.tag, item.fileName, destination, MoveUnSuccessful)
    }
  }
}
