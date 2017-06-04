package services


import java.io.FileNotFoundException

import better.files.File
import domains.{BBCTags, PodcastItem}
import domains.OutCome._
import org.jaudiotagger.tag.FieldKey
import org.scalatest.Matchers._
import org.scalatest.{BeforeAndAfterEach, FlatSpec}
import org.scalatest.concurrent.ScalaFutures



/**
  * Created by Nenyi on 23/03/2017.
  */
class SimplePodcastToolsTest extends FlatSpec with testData with ScalaFutures with BeforeAndAfterEach {
  var s: SimplePodcastTools = _
  s = new SimplePodcastTools

  override def beforeEach() {
    setupTestResources
  }

  override def afterEach() {
    teardownTestResources
  }

  "getPodcasts" should "be able to create PodcastItem from valid file" in {
    whenReady(s getPodcasts (List(audioFile3))) {
      results =>
        results.size shouldBe 1
        val item = results.head
        item.podcastFile.getAbsoluteFile shouldBe audioFile3.getAbsoluteFile
        item.fileName shouldBe audioFile3.getName
        item.destDir shouldBe ""
        val tag = item.tag
        s extractField(tag, FieldKey.ARTIST) shouldBe "BBC Radio 1Xtra"
        s extractField(tag, FieldKey.ORIGINAL_ARTIST) shouldBe ""
        s extractField(tag, FieldKey.TITLE) shouldBe "David Rodigan: Kirkledove"
        s extractField(tag, FieldKey.ALBUM) shouldBe "David Rodigan"
        s extractField(tag, FieldKey.ALBUM_ARTIST) shouldBe "BBC Radio"
        s extractField(tag, FieldKey.GENRE) shouldBe "Music"
        s extractField(tag, FieldKey.COMPOSER) shouldBe "BBC iPlayer"
    }
  }
  it should "be empty if file is not a valid" in {
    whenReady(s getPodcasts (List(audioFile2))) {
      results => results.isEmpty shouldBe true
    }
  }

  "mapPodcastDestination" should "be able to map destination to new podcast" in {
    whenReady(s getPodcasts (List(audioFile6))) {
      results =>
        val item = s mapPodcastDestination (
          s getPodcastDestination(results.head, defaultSettings.destination))
        item.destDir shouldBe destination + getSeparator + "(BBC Radio 1)-Monki/1.0"
    }
  }

  "renameFile" should "" in {
    val src = audioFile6.getName

    whenReady(s getPodcasts (List(audioFile6))) {
      results =>
        whenReady(
          s renameFilenameFromTags(results.head, Seq(FieldKey.ARTIST, FieldKey.TITLE), "-")) {
          item =>
            val renamedItem = s renameFile (item)
            renamedItem.fileName shouldBe "(Monki)-Lee Foss Live Lights On Mix.m4a"
            val originalItem = new PodcastItem(renamedItem.podcastFile, renamedItem.tag, src, renamedItem.destDir, renamedItem.outCome)
            s renameFile (originalItem)
        }
    }

  }

  "movePodcast" should "move podcast to destination" in {
    val result = s movePodcast (finalItem)
    val finalDestination = result.podcastFile.getParent
    finalItem.destDir shouldBe finalDestination
    s move(sourceFinalItem, destinationFinalItem)
  }
  it should "throw an FileExistsException" in {
    val result = s movePodcast (finalItem)
    val renamedFinalItemCopy = s renameFile (finalItemCopy)
    val failed = s movePodcast (renamedFinalItemCopy)
    failed.outCome shouldBe MoveUnSuccessful
    renamedFinalItemCopy.podcastFile.renameTo(destinationFinalItemCopy)
    s move(sourceFinalItem, destinationFinalItem)
  }

  "doesPodcastExist" should "be able to find matching podcasts and return 1" in {
    whenReady(s getPodcasts (List(audioFile6))) {
      podcastItems =>
        val tags = podcastItems.map(_.tag)
        s doesPodcastExist("Lee Foss Live Lights On Mix", tags) shouldBe 1
    }
  }
  it should "return 1 if there is a partial match" in {
    whenReady(s getPodcasts (List(audioFile6))) {
      podcastItems =>
        val tags = podcastItems.map(_.tag)
        s doesPodcastExist("Lee Foss Live Lights", tags) shouldBe 1
    }
  }
  it should "return 0 if there are no matching podcasts" in {
    whenReady(s getPodcasts (List(audioFile6))) {
      podcastItems =>
        val tags = podcastItems.map(_.tag)
        s doesPodcastExist("test title", tags) shouldBe 0
    }
  }

  "renamePodcastTags" should "be able to rename podcast to something of my choosing" in {
    whenReady(s getPodcasts (List(audioFile6))) {
      podcastItems =>
        val item = podcastItems.head
        val expectedTags = new BBCTags("Artist", "Title", "Album", "Album", "Genre")
        val tag = s renamePodcastTags(item.tag, expectedTags)
        s extractField(tag, FieldKey.ARTIST) shouldBe "Artist"
        s extractField(tag, FieldKey.ORIGINAL_ARTIST) shouldBe "Artist"
        s extractField(tag, FieldKey.TITLE) shouldBe "Title"
        s extractField(tag, FieldKey.ALBUM) shouldBe "Album"
        s extractField(tag, FieldKey.ALBUM_ARTIST) shouldBe "Album"
        s extractField(tag, FieldKey.GENRE) shouldBe "Genre"
    }
  }

  "retagIfPodcastExist" should "not rename tags if there are no existing podcasts of siimilar name" in {
    whenReady(s getPodcasts (List(audioFile4, audioFile1, audioFile3, audioFile5))) {
      podcastItems =>
        val item = podcastItems.head
        val itemTag = item.tag
        val existingPodcastItems = podcastItems.tail
        val result = s retagIfPodcastExist(item, existingPodcastItems.map(_.tag))
        val resultTag = result.tag

        result.destDir shouldBe item.destDir
        result.fileName shouldBe item.fileName
        result.podcastFile shouldBe item.podcastFile

        s extractField(resultTag, FieldKey.ARTIST) shouldBe s.extractField(itemTag, FieldKey.ARTIST)
        s extractField(resultTag, FieldKey.ORIGINAL_ARTIST) shouldBe s.extractField(itemTag, FieldKey.ORIGINAL_ARTIST)
        s extractField(resultTag, FieldKey.TITLE) shouldBe s.extractField(itemTag, FieldKey.TITLE)
        s extractField(resultTag, FieldKey.ALBUM) shouldBe s.extractField(itemTag, FieldKey.ALBUM)
        s extractField(resultTag, FieldKey.ALBUM_ARTIST) shouldBe s.extractField(itemTag, FieldKey.ALBUM_ARTIST)
        s extractField(resultTag, FieldKey.GENRE) shouldBe s.extractField(itemTag, FieldKey.GENRE)
    }
  }
  it should "rename tags if there are existing podcasts of similar name" in {
    whenReady(s getPodcasts (List(audioFile6, audioFile1, audioFile7, audioFile5))) {
      podcastItems =>
        val item = podcastItems.head
        val timeTag = item.tag
        val existingPodcastItems = podcastItems.tail
        val result = s retagIfPodcastExist(item, existingPodcastItems.map(_.tag))
        val resultTag = result.tag

        result.destDir shouldBe item.destDir
        result.fileName shouldBe "(Monki)-Lee Foss Live Lights On Mix 2"
        result.podcastFile shouldBe item.podcastFile

        s extractField(resultTag, FieldKey.ARTIST) shouldBe "(Monki)"
        s extractField(resultTag, FieldKey.ORIGINAL_ARTIST) shouldBe "(Monki)"
        s extractField(resultTag, FieldKey.TITLE) shouldBe "Lee Foss Live Lights On Mix 2"
        s extractField(resultTag, FieldKey.ALBUM) shouldBe s.extractField(timeTag, FieldKey.ALBUM)
        s extractField(resultTag, FieldKey.ALBUM_ARTIST) shouldBe s.extractField(timeTag, FieldKey.ALBUM_ARTIST)
        s extractField(resultTag, FieldKey.GENRE) shouldBe s.extractField(timeTag, FieldKey.GENRE)
    }
  }

  "traverseExistingAndNewPodcasts" should "be able to work through a list of new podcasts against existing podcasts that do not match and return the same as the original" in {
    whenReady(s getPodcasts (List(audioFile1, audioFile4))) {
      newItems =>
        whenReady(s getPodcasts (List(audioFile2, audioFile3, audioFile5, audioFile6, audioFile7))) {
          existingItems =>
            val firstItem = newItems.filter(_.fileName == "podcast1.m4a").head
            val firstTag = firstItem.tag
            val secondItem = newItems.filter(_.fileName == "podcast4.m4a").head
            val secondTag = secondItem.tag
            val resultingItems = s traverseExistingAndNewPodcasts(newItems, existingItems.map(_.tag))
            resultingItems.size shouldBe 2
            val firstResult = resultingItems.filter(_.fileName == "podcast1.m4a").head
            val firstResultTag = firstResult.tag
            val secondResult = resultingItems.filter(_.fileName == "podcast4.m4a").head
            val secondResultTag = secondResult.tag

            firstResult.destDir shouldBe firstItem.destDir
            firstResult.fileName shouldBe firstItem.fileName
            firstResult.podcastFile shouldBe firstItem.podcastFile
            s extractField(firstResultTag, FieldKey.ARTIST) shouldBe s.extractField(firstTag, FieldKey.ARTIST)
            s extractField(firstResultTag, FieldKey.ORIGINAL_ARTIST) shouldBe s.extractField(firstTag, FieldKey.ORIGINAL_ARTIST)
            s extractField(firstResultTag, FieldKey.TITLE) shouldBe s.extractField(firstTag, FieldKey.TITLE)
            s extractField(firstResultTag, FieldKey.ALBUM) shouldBe s.extractField(firstTag, FieldKey.ALBUM)
            s extractField(firstResultTag, FieldKey.ALBUM_ARTIST) shouldBe s.extractField(firstTag, FieldKey.ALBUM_ARTIST)
            s extractField(firstResultTag, FieldKey.GENRE) shouldBe s.extractField(firstTag, FieldKey.GENRE)


            secondResult.destDir shouldBe secondItem.destDir
            secondResult.fileName shouldBe secondItem.fileName
            secondResult.podcastFile shouldBe secondItem.podcastFile
            s extractField(secondResultTag, FieldKey.ARTIST) shouldBe s.extractField(secondTag, FieldKey.ARTIST)
            s extractField(secondResultTag, FieldKey.ORIGINAL_ARTIST) shouldBe s.extractField(secondTag, FieldKey.ORIGINAL_ARTIST)
            s extractField(secondResultTag, FieldKey.TITLE) shouldBe s.extractField(secondTag, FieldKey.TITLE)
            s extractField(secondResultTag, FieldKey.ALBUM) shouldBe s.extractField(secondTag, FieldKey.ALBUM)
            s extractField(secondResultTag, FieldKey.ALBUM_ARTIST) shouldBe s.extractField(secondTag, FieldKey.ALBUM_ARTIST)
            s extractField(secondResultTag, FieldKey.GENRE) shouldBe s.extractField(secondTag, FieldKey.GENRE)

        }
    }
  }
  it should "be able to work through a list of new podcasts against existing podcasts that match and return one new item" in {
    whenReady(s getPodcasts (List(audioFile6, audioFile4))) {
      newItems =>
        whenReady(s getPodcasts (List(audioFile1, audioFile2, audioFile3, audioFile5, audioFile6, audioFile7))) {
          existingItems =>
            val firstItem = newItems.filter(_.fileName == "podcast4.m4a").head
            val firstTag = firstItem.tag
            val secondItem = newItems.filter(_.fileName == "podcast6.m4a").head
            val secondTag = secondItem.tag
            val resultingItems = s traverseExistingAndNewPodcasts(newItems, existingItems.map(_.tag))
            resultingItems.size shouldBe 2
            val firstResult = resultingItems.filter(_.fileName == "podcast4.m4a").head
            val firstResultTag = firstResult.tag
            val secondResult = resultingItems.filter(_.fileName != "podcast4.m4a").head
            val secondResultTag = secondResult.tag

            firstResult.destDir shouldBe firstItem.destDir
            firstResult.fileName shouldBe firstItem.fileName
            firstResult.podcastFile shouldBe firstItem.podcastFile
            s extractField(firstResultTag, FieldKey.ARTIST) shouldBe s.extractField(firstTag, FieldKey.ARTIST)
            s extractField(firstResultTag, FieldKey.ORIGINAL_ARTIST) shouldBe s.extractField(firstTag, FieldKey.ORIGINAL_ARTIST)
            s extractField(firstResultTag, FieldKey.TITLE) shouldBe s.extractField(firstTag, FieldKey.TITLE)
            s extractField(firstResultTag, FieldKey.ALBUM) shouldBe s.extractField(firstTag, FieldKey.ALBUM)
            s extractField(firstResultTag, FieldKey.ALBUM_ARTIST) shouldBe s.extractField(firstTag, FieldKey.ALBUM_ARTIST)
            s extractField(firstResultTag, FieldKey.GENRE) shouldBe s.extractField(firstTag, FieldKey.GENRE)


            secondResult.destDir shouldBe secondItem.destDir
            secondResult.fileName shouldBe "(Monki)-Lee Foss Live Lights On Mix 1"
            secondResult.podcastFile shouldBe secondItem.podcastFile
            s extractField(secondResultTag, FieldKey.ARTIST) shouldBe "(Monki)"
            s extractField(secondResultTag, FieldKey.ORIGINAL_ARTIST) shouldBe "(Monki)"
            s extractField(secondResultTag, FieldKey.TITLE) shouldBe "Lee Foss Live Lights On Mix 1"
            s extractField(secondResultTag, FieldKey.ALBUM) shouldBe s.extractField(secondTag, FieldKey.ALBUM)
            s extractField(secondResultTag, FieldKey.ALBUM_ARTIST) shouldBe s.extractField(secondTag, FieldKey.ALBUM_ARTIST)
            s extractField(secondResultTag, FieldKey.GENRE) shouldBe s.extractField(secondTag, FieldKey.GENRE)
        }
    }
  }

  "renameExistingPodcasts" should "be able to find existing podcasts and rename matching new podcasts" in {
    whenReady(s getPodcasts (List(audioFile6, audioFile4))) {
      newItems =>

        val items = newItems.map(n => s getPodcastDestination(n, defaultSettings.destination))

        val firstItem = items.filter(_.fileName == "podcast4.m4a").head
        val firstTag = firstItem.tag
        val secondItem = items.filter(_.fileName == "podcast6.m4a").head
        val secondTag = secondItem.tag
        val resultingItems = s renameExistingPodcasts(items, defaultSettings.extensions)
        resultingItems.size shouldBe 2
        val firstResult = resultingItems.filter(_.fileName == "podcast4.m4a").head
        val firstResultTag = firstResult.tag
        val secondResult = resultingItems.filter(_.fileName != "podcast4.m4a").head
        val secondResultTag = secondResult.tag

        firstResult.destDir shouldBe firstItem.destDir
        firstResult.fileName shouldBe firstItem.fileName
        firstResult.podcastFile shouldBe firstItem.podcastFile
        s extractField(firstResultTag, FieldKey.ARTIST) shouldBe s.extractField(firstTag, FieldKey.ARTIST)
        s extractField(firstResultTag, FieldKey.ORIGINAL_ARTIST) shouldBe s.extractField(firstTag, FieldKey.ORIGINAL_ARTIST)
        s extractField(firstResultTag, FieldKey.TITLE) shouldBe s.extractField(firstTag, FieldKey.TITLE)
        s extractField(firstResultTag, FieldKey.ALBUM) shouldBe s.extractField(firstTag, FieldKey.ALBUM)
        s extractField(firstResultTag, FieldKey.ALBUM_ARTIST) shouldBe s.extractField(firstTag, FieldKey.ALBUM_ARTIST)
        s extractField(firstResultTag, FieldKey.GENRE) shouldBe s.extractField(firstTag, FieldKey.GENRE)

        val secondTagGenre = s.extractField(secondTag, FieldKey.GENRE)
        val secondResultGenre = s extractField(secondResultTag, FieldKey.GENRE)
        secondResult.destDir shouldBe secondItem.destDir
        secondResult.fileName shouldBe "(Monki)-Lee Foss Live Lights On Mix 2"
        secondResult.podcastFile shouldBe secondItem.podcastFile
        s extractField(secondResultTag, FieldKey.ARTIST) shouldBe "(Monki)"
        s extractField(secondResultTag, FieldKey.ORIGINAL_ARTIST) shouldBe "(Monki)"
        s extractField(secondResultTag, FieldKey.TITLE) shouldBe "Lee Foss Live Lights On Mix 2"
        s extractField(secondResultTag, FieldKey.ALBUM) shouldBe s.extractField(secondTag, FieldKey.ALBUM)
        s extractField(secondResultTag, FieldKey.ALBUM_ARTIST) shouldBe s.extractField(secondTag, FieldKey.ALBUM_ARTIST)
        secondResultGenre shouldBe secondTagGenre
    }
  }

  "process" should "throw and error if default settings are non existant directories" in {
    intercept[FileNotFoundException](s process(defaultSettingsWithIncorrectSourceAndDestination))
  }
  it should "be able to move all real podacasts to destination" in {
    val results = s process(defaultSettings)
    results.map(r => r.outCome shouldBe MoveSuccessful)
  }


  //  "getPodcasts" should "all files that have either been unprocessed" in {
  //    //this is where I will have to start looking into the file
  //    for(podcast <- s getPodcastFiles(defaultsettings)) { podcast shouldBe true }
  //  }
}
