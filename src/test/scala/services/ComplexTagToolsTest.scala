package services

import domains._
import org.jaudiotagger.tag.{FieldKey, Tag, TagField}
import org.scalamock.scalatest.MockFactory
import org.scalatest.Matchers._
import org.scalatest.{BeforeAndAfter, BeforeAndAfterEach, FlatSpec}
import org.scalatest.concurrent.ScalaFutures

import scala.util.matching.Regex

/**
  * Created by Nenyi on 09/04/2017.
  */
class ComplexTagToolsTest extends FlatSpec with BeforeAndAfter with ComplexTagTools with BeforeAndAfterEach
  with MockFactory with testData with ScalaFutures {

  override def beforeEach() {
    setupTestResources
  }

  override def afterEach() {
    teardownTestResources
  }

  "isUnprocessedBBCPodcast" should "be able to handle a Future list of podcasts" in {
    val podcastFiles = List(audioFile1, audioFile2, audioFile3, audioFile4, audioFile5, audioFile6)
    whenReady(isUnprocessedBBCPodcast(constructPodcastItems(podcastFiles))) {
      result => result.size shouldBe 4
    }
  }

  "isProcessedBBCPodcast" should "be able to handle a Future list of podcasts" in {
    val podcastFiles = List(audioFile1, audioFile2, audioFile3, audioFile4, audioFile5, audioFile6)
    whenReady(isProcessedBBCPodcast(constructPodcastItems(podcastFiles))) {
      result => result.size shouldBe 1
    }
  }

  "constructPodcastItems" should "be able to get a valid list of tags from a list of files." in {
    val podcastFiles = List(audioFile1, audioFile2, audioFile3, audioFile4, audioFile5)
    whenReady(constructPodcastItems(podcastFiles)) { result => result.size shouldBe 4 }
  }

  it should "be able to get a list of unprocessed podcasts tags from a list of files." in {
    val podcastFiles = List(audioFile1, audioFile2, audioFile3, audioFile4, audioFile5)
    whenReady(constructPodcastItems(podcastFiles)) { result =>
      val unprocessed = result.filter(isUnprocessedBBCPodcast(_))
      unprocessed.size shouldBe 4
    }
  }

  it should "be able to get a list of processed podcasts tags from a list of files." in {
    val podcastFiles = List(audioFile1, audioFile2, audioFile3, audioFile4, audioFile5, audioFile6)
    whenReady(constructPodcastItems(podcastFiles)) { result =>
      val processed = result.filter(isProcessedBBCPodcast(_))
      processed.size shouldBe 1
    }
  }

  "Tagging" should "be able to extract the artist and title from an unprocessed podcast title" in {
    whenReady(extractPodcastTagsFuture(audioFile5)) {
      result =>
        val combinedResults = extractArtistAndTitleTagField(result.get, ": ")
        combinedResults("artist") shouldBe "Monki"
        combinedResults("title") shouldBe "Lee Foss Live Lights On Mix"
    }
  }

  it should "be able to rename the artist to standard format" in {
    whenReady(extractPodcastTagsFuture(audioFile7)) {
      result =>
        val combinedResults = extractArtistAndTitleTagField(result.get, ": ")
        val list = List(RenamePattern(new Regex("\\A"), "("), RenamePattern(new Regex("\\z"), ")"))
        renameArtist(combinedResults("artist"), list) shouldBe "(Monki)"
    }
  }

  "TransformPodcast" should "be able to convert a podcast into my favourite format" in {
    val transforms: Seq[(Tag) => Tag] = Seq(copyField(_, FieldKey.TITLE, FieldKey.ARTIST),
      renameField(_, FieldKey.ARTIST, renameArtistPattern), copyField(_, FieldKey.ARTIST, FieldKey.ORIGINAL_ARTIST),
      renameField(_, FieldKey.TITLE, renameTitlePattern), renameField(_, FieldKey.ALBUM, renameAlbumPattern),
      copyField(_, FieldKey.ALBUM, FieldKey.ALBUM_ARTIST), renameField(_, FieldKey.GENRE, renameGenrePattern))

    whenReady(constructPodcastItem(audioFile5)) { result =>
      whenReady(TransformPodcastTags(result.get, transforms)) {
        transformedItem =>
          transformedItem.tag.getFirst(FieldKey.ARTIST) shouldBe "(Monki)"
          transformedItem.tag.getFirst(FieldKey.ORIGINAL_ARTIST) shouldBe "(Monki)"
          transformedItem.tag.getFirst(FieldKey.TITLE) shouldBe "Lee Foss Live Lights On Mix"
          transformedItem.tag.getFirst(FieldKey.ALBUM) shouldBe "(BBC Radio 1)-Monki"
          transformedItem.tag.getFirst(FieldKey.ALBUM_ARTIST) shouldBe "(BBC Radio 1)-Monki"
          transformedItem.tag.getFirst(FieldKey.GENRE) shouldBe "Podcast"
          transformedItem.tag.getFirst(FieldKey.COMPOSER) shouldBe "BBC iPlayer"
      }
    }
  }
  it should "be able to produce a transform for the new format" in {
    val transforms: Seq[(Tag) => Tag] = Seq(copyField(_, FieldKey.ALBUM, FieldKey.ARTIST),
      removeFieldFromField(_, FieldKey.ARTIST, FieldKey.TITLE),
      renameField(_, FieldKey.ARTIST, renameArtistPattern), copyField(_, FieldKey.ARTIST, FieldKey.ORIGINAL_ARTIST),
      renameField(_, FieldKey.ALBUM, renameAlbumPattern), copyField(_, FieldKey.ALBUM, FieldKey.ALBUM_ARTIST),
      renameField(_, FieldKey.GENRE, renameGenrePattern))
    whenReady(constructPodcastItem(audioFileWithNewFormat1)) { result =>
      whenReady(TransformPodcastTags(result.get, transforms)) {
        transformedItem =>
          transformedItem.tag.getFirst(FieldKey.ARTIST) shouldBe "(B.Traits)"
          transformedItem.tag.getFirst(FieldKey.ORIGINAL_ARTIST) shouldBe "(B.Traits)"
          transformedItem.tag.getFirst(FieldKey.TITLE) shouldBe "Jeff Mills Special"
          transformedItem.tag.getFirst(FieldKey.ALBUM) shouldBe "(BBC Radio 1)-B.Traits"
          transformedItem.tag.getFirst(FieldKey.ALBUM_ARTIST) shouldBe "(BBC Radio 1)-B.Traits"
          transformedItem.tag.getFirst(FieldKey.GENRE) shouldBe "Podcast"
          transformedItem.tag.getFirst(FieldKey.COMPOSER) shouldBe "BBC iPlayer"
      }
    }
  }
  it should "be able to produce a transform for the Chris Cornell format" in {
    val transforms: Seq[(Tag) => Tag] = Seq(copyField(_, FieldKey.ALBUM, FieldKey.ARTIST),
      splitField(_, FieldKey.ARTIST, ":", 0), splitField(_, FieldKey.ALBUM, ":", 0),
      removeFieldFromField(_, FieldKey.ARTIST, FieldKey.TITLE),
      renameField(_, FieldKey.ARTIST, renameArtistPattern), copyField(_, FieldKey.ARTIST, FieldKey.ORIGINAL_ARTIST),
      renameField(_, FieldKey.ALBUM, renameAlbumPattern), copyField(_, FieldKey.ALBUM, FieldKey.ALBUM_ARTIST),
      renameField(_, FieldKey.GENRE, renameGenrePattern))
    whenReady(constructPodcastItem(audioFileWithNewFormat2)) { result =>
      whenReady(TransformPodcastTags(result.get, transforms)) {
        transformedItem =>
          transformedItem.tag.getFirst(FieldKey.ARTIST) shouldBe "(Chris Cornell)"
          transformedItem.tag.getFirst(FieldKey.ORIGINAL_ARTIST) shouldBe "(Chris Cornell)"
          transformedItem.tag.getFirst(FieldKey.TITLE) shouldBe "A 6 Music Tribute"
          transformedItem.tag.getFirst(FieldKey.ALBUM) shouldBe "(BBC Radio 1)-Chris Cornell"
          transformedItem.tag.getFirst(FieldKey.ALBUM_ARTIST) shouldBe "(BBC Radio 1)-Chris Cornell"
          transformedItem.tag.getFirst(FieldKey.GENRE) shouldBe "Podcast"
          transformedItem.tag.getFirst(FieldKey.COMPOSER) shouldBe "BBC iPlayer"
      }
    }
  }
  it should "be able to use the new transform sequence to work with old podcasts (pre April 2017)" in {
    val transforms: Seq[(Tag) => Tag] = Seq(copyField(_, FieldKey.ALBUM, FieldKey.ARTIST),
      removeFieldFromField(_, FieldKey.ARTIST, FieldKey.TITLE),
      renameField(_, FieldKey.ARTIST, renameArtistPattern), copyField(_, FieldKey.ARTIST, FieldKey.ORIGINAL_ARTIST),
      renameField(_, FieldKey.ALBUM, renameAlbumPattern), copyField(_, FieldKey.ALBUM, FieldKey.ALBUM_ARTIST),
      renameField(_, FieldKey.GENRE, renameGenrePattern))

    whenReady(constructPodcastItem(audioFile5)) { result =>
      whenReady(TransformPodcastTags(result.get, transforms)) {
        transformedItem =>
          transformedItem.tag.getFirst(FieldKey.ARTIST) shouldBe "(Monki)"
          transformedItem.tag.getFirst(FieldKey.ORIGINAL_ARTIST) shouldBe "(Monki)"
          transformedItem.tag.getFirst(FieldKey.TITLE) shouldBe "Lee Foss Live Lights On Mix"
          transformedItem.tag.getFirst(FieldKey.ALBUM) shouldBe "(BBC Radio 1)-Monki"
          transformedItem.tag.getFirst(FieldKey.ALBUM_ARTIST) shouldBe "(BBC Radio 1)-Monki"
          transformedItem.tag.getFirst(FieldKey.GENRE) shouldBe "Podcast"
          transformedItem.tag.getFirst(FieldKey.COMPOSER) shouldBe "BBC iPlayer"
      }
    }
  }

  "renameFilenameFromTags" should "be able to rename file to artist name format" in {
    whenReady(constructPodcastItem(audioFile6)) {
      result =>
        whenReady(renameFilenameFromTags(result.get, Seq(FieldKey.ARTIST, FieldKey.TITLE), "-")) {
          transformedItem => transformedItem.fileName shouldBe "(Monki)-Lee Foss Live Lights On Mix"
        }
    }
  }

  "writeToPodcast" should "be able to save tags to file" in {
    val transforms: Seq[(Tag) => Tag] = Seq(copyField(_, FieldKey.TITLE, FieldKey.ARTIST),
      renameField(_, FieldKey.ARTIST, renameArtistPattern), copyField(_, FieldKey.ARTIST, FieldKey.ORIGINAL_ARTIST),
      renameField(_, FieldKey.TITLE, renameTitlePattern), renameField(_, FieldKey.ALBUM, renameAlbumPattern),
      copyField(_, FieldKey.ALBUM, FieldKey.ALBUM_ARTIST), renameField(_, FieldKey.GENRE, renameGenrePattern))

    whenReady(constructPodcastItem(audioFile5)) { result =>
      whenReady(TransformPodcastTags(result.get, transforms)) { transformedItem =>
        val item = writeToPodcast(transformedItem)
        whenReady(constructPodcastItem(item.podcastFile)) { subResult =>
          val newItem = subResult.get
          newItem.tag.getFirst(FieldKey.ARTIST) shouldBe "(Monki)"
          newItem.tag.getFirst(FieldKey.ORIGINAL_ARTIST) shouldBe "(Monki)"
          newItem.tag.getFirst(FieldKey.TITLE) shouldBe "Lee Foss Live Lights On Mix"
          newItem.tag.getFirst(FieldKey.ALBUM) shouldBe "(BBC Radio 1)-Monki"
          newItem.tag.getFirst(FieldKey.ALBUM_ARTIST) shouldBe "(BBC Radio 1)-Monki"
          newItem.tag.getFirst(FieldKey.GENRE) shouldBe "Podcast"
          newItem.tag.getFirst(FieldKey.COMPOSER) shouldBe "BBC iPlayer"
        }
      }
    }
  }

  "getPodcastDestination" should "find a destination from a constructed PodcastItem" in {
    whenReady(constructPodcastItem(audioFile6)) { result =>
      val item = getPodcastDestination(result.get, defaultSettings.destination)
      item.destDir shouldBe destination + getSeparator + "(BBC Radio 1)-Monki"
    }
  }

}
