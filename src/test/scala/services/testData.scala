package services

import java.io.File
import java.util.logging.{Level, Logger}

import better.files.{File => BetterFile}
import domains.{PodSettings, PodcastItem, RenamePattern}
import domains.OutCome._
import org.jaudiotagger.audio.AudioFileIO
import org.jaudiotagger.tag.FieldKey
import org.scalamock.scalatest.MockFactory
import spray.json.{JsArray, JsObject, JsString}

import scala.util.matching.Regex

/**
  * Created by Nenyi on 23/03/2017.
  */
trait testData extends MockFactory {
  val getSeparator = File.separator
  val source = getClass.getResource(getSeparator + "source").getPath
  val destination = getClass.getResource(getSeparator + "destination").getPath

  val sourceCopy = source + "copy"
  val destinationCopy = destination + "copy"

  val sourceRandom = source + "random"
  val destinationRandom = destination + "random"

  val destinationPodcast = destination + getSeparator + "podcast"
  val sourcePodcast = source + getSeparator + "podcast"

  val defaultSettings = new PodSettings(source, destination, Array("m4a"))
  val defaultSettingsPodcast = new PodSettings(sourcePodcast, destinationPodcast, Array("m4a"))
  val defaultSettingsWithIncorrectSourceAndDestination = new PodSettings(sourceRandom, destinationRandom, Array("m4a"))
  private val liveSource = "/Users/Nenyi/Music/podcasts"
  private val liveDestination = "/Users/Nenyi/Downloads/podcasts"
  val liveSettings = new PodSettings(liveSource, liveDestination, Array("m4a"))


  val testJSON = JsObject("source" -> JsString(source),
    "destination" -> JsString(destination), "extensions" -> JsArray(JsString("m4a")))
  val resourceSettings = new PodSettings(source, destination, Array("m4a"))
  val actualDestination = destination + getSeparator + "(BBC Radio 1)-Monki"

  val renameArtistPattern = List(RenamePattern(new Regex(": (.*)"), ""), RenamePattern(new Regex("\\A"), "("), RenamePattern(new Regex("\\z"), ")"))
  val renameTitlePattern = List(RenamePattern(new Regex("(.*): "), ""))
  val renameAlbumPattern = List(RenamePattern(new Regex("\\A"), "(BBC Radio 1)-"))
  val renameGenrePattern = List(RenamePattern(new Regex("[^\\n]+"), "Podcast"))

  def audioFile1 = {
    Logger.getLogger("org").setLevel(Level.OFF)
    val audioFile = AudioFileIO.read(new File(source + getSeparator + "podcast1.m4a"))
    val podcastTag1 = audioFile.getTag()
    podcastTag1.setField(FieldKey.ARTIST, "BBC Radio 1Xtra")
    podcastTag1.setField(FieldKey.ORIGINAL_ARTIST, "")
    podcastTag1.setField(FieldKey.TITLE, "Diplo and Friends: Diplo")
    podcastTag1.setField(FieldKey.ALBUM, "Diplo and Friends")
    podcastTag1.setField(FieldKey.ALBUM_ARTIST, "BBC Radio")
    podcastTag1.setField(FieldKey.GENRE, "Music")
    podcastTag1.setField(FieldKey.COMPOSER, "BBC iPlayer")
    audioFile.setTag(podcastTag1)
    audioFile.commit()
    audioFile.getFile
  }

  def audioFile2 = new File(source + getSeparator + "podcast3.m4a")

  def audioFile3 = new File(source + getSeparator + "realpodcast.m4a")

  def audioFile4 = {
    Logger.getLogger("org").setLevel(Level.OFF)
    val audioFile = AudioFileIO.read(new File(source + getSeparator +  "podcast4.m4a"))
    val podcastTag1 = audioFile.getTag()
    podcastTag1.setField(FieldKey.ARTIST, "BBC Radio 6 Music")
    podcastTag1.setField(FieldKey.ORIGINAL_ARTIST, "")
    podcastTag1.setField(FieldKey.TITLE, "Mary Anne Hobbs: With Ian Rankin")
    podcastTag1.setField(FieldKey.ALBUM, "Mary Anne Hobbs")
    podcastTag1.setField(FieldKey.ALBUM_ARTIST, "Mary Anne Hobbs")
    podcastTag1.setField(FieldKey.GENRE, "Music")
    podcastTag1.setField(FieldKey.COMPOSER, "BBC iPlayer")
    audioFile.setTag(podcastTag1)
    audioFile.commit()
    audioFile.getFile
  }

  def audioFile5 = {
    Logger.getLogger("org").setLevel(Level.OFF)
    val audioFile = AudioFileIO.read(new File(source + getSeparator + "podcast5.m4a"))
    val podcastTag1 = audioFile.getTag()
    podcastTag1.setField(FieldKey.ARTIST, "BBC Radio 1Xtra")
    podcastTag1.setField(FieldKey.ORIGINAL_ARTIST, "")
    podcastTag1.setField(FieldKey.TITLE, "Monki: Lee Foss Live Lights On Mix")
    podcastTag1.setField(FieldKey.ALBUM, "Monki")
    podcastTag1.setField(FieldKey.ALBUM_ARTIST, "BBC Radio")
    podcastTag1.setField(FieldKey.GENRE, "Music")
    podcastTag1.setField(FieldKey.COMPOSER, "BBC iPlayer")
    audioFile.setTag(podcastTag1)
    audioFile.commit()
    audioFile.getFile
  }

  def audioFile6 = {
    Logger.getLogger("org").setLevel(Level.OFF)
    val audioFile = AudioFileIO.read(new File(source + getSeparator + "podcast6.m4a"))
    val podcastTag1 = audioFile.getTag()
    podcastTag1.setField(FieldKey.ARTIST, "(Monki)")
    podcastTag1.setField(FieldKey.ORIGINAL_ARTIST, "(Monki)")
    podcastTag1.setField(FieldKey.TITLE, "Lee Foss Live Lights On Mix")
    podcastTag1.setField(FieldKey.ALBUM, "(BBC Radio 1)-Monki")
    podcastTag1.setField(FieldKey.ALBUM_ARTIST, "(BBC Radio 1)-Monki")
    podcastTag1.setField(FieldKey.GENRE, "Podcast")
    podcastTag1.setField(FieldKey.COMPOSER, "BBC iPlayer")
    audioFile.setTag(podcastTag1)
    audioFile.commit()
    audioFile.getFile
  }

  def audioFile7 = {
    Logger.getLogger("org").setLevel(Level.OFF)
    val audioFile = AudioFileIO.read(new File(source + getSeparator + "podcast5.m4a"))
    val podcastTag1 = audioFile.getTag()
    podcastTag1.setField(FieldKey.ARTIST, "Monki: Lee Foss Live Lights On Mix")
    podcastTag1.setField(FieldKey.ORIGINAL_ARTIST, "")
    podcastTag1.setField(FieldKey.TITLE, "Monki: Lee Foss Live Lights On Mix")
    podcastTag1.setField(FieldKey.ALBUM, "Monki")
    podcastTag1.setField(FieldKey.ALBUM_ARTIST, "BBC Radio")
    podcastTag1.setField(FieldKey.GENRE, "Music")
    podcastTag1.setField(FieldKey.COMPOSER, "BBC iPlayer")
    audioFile.setTag(podcastTag1)
    audioFile.commit()
    audioFile.getFile
  }

  def audioFileWithNewFormat1 = new File(source + getSeparator + "podcast7.m4a")

  def audioFileWithNewFormat2 = {
    Logger.getLogger("org").setLevel(Level.OFF)
    val audioFile = AudioFileIO.read(new File(source + getSeparator + "podcast7.m4a"))
    val podcastTag1 = audioFile.getTag()
    podcastTag1.setField(FieldKey.ARTIST, "BBC Radio 6 Music")
    podcastTag1.setField(FieldKey.ORIGINAL_ARTIST, "")
    podcastTag1.setField(FieldKey.TITLE, "Chris Cornell: A 6 Music Tribute")
    podcastTag1.setField(FieldKey.ALBUM, "Chris Cornell: A 6 Music Tribute")
    podcastTag1.setField(FieldKey.ALBUM_ARTIST, "BBC Radio")
    podcastTag1.setField(FieldKey.GENRE, "Music")
    podcastTag1.setField(FieldKey.COMPOSER, "BBC iPlayer")
    audioFile.setTag(podcastTag1)
    audioFile.commit()
    audioFile.getFile
  }


  val finalItem = new PodcastItem(audioFile6, AudioFileIO.read(audioFile6).getTag, audioFile6.getName, destinationPodcast, FileRenamed)
  val finalItemCopy = new PodcastItem(audioFile5, AudioFileIO.read(audioFile6).getTag, "podcast6", destinationPodcast, FileRenamed)
  val sourceFinalItem = finalItem.destDir + getSeparator + finalItem.fileName
  val destinationFinalItem = finalItem.podcastFile.getParent
  val destinationFinalItemCopy = finalItemCopy.podcastFile.getAbsoluteFile

  private val srcCopy = BetterFile(sourceCopy)
  private val destCopy = BetterFile(destinationCopy)

  private val src = BetterFile(source)
  private val dest = BetterFile(destination)

  def setupTestResources: Unit = {
    dest.copyTo(destCopy, true)
    src.copyTo(srcCopy, true)
  }

  def teardownTestResources: Unit  = {
    destCopy.copyTo(dest, true)
    destCopy.delete(false)
    srcCopy.copyTo(src, true)
    srcCopy.delete(false)
  }
}
