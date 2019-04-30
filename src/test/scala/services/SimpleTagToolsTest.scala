package services

import java.io.File

import domains._
import org.jaudiotagger.tag.{FieldKey, Tag, TagField}
import org.scalamock.scalatest.MockFactory
import org.scalatest.Matchers._
import org.scalatest.{BeforeAndAfter, BeforeAndAfterEach, FlatSpec}
import org.scalatest.concurrent.ScalaFutures

import scala.util.matching.Regex

/**
  * Created by Nenyi on 24/03/2017.
  */
class SimpleTagToolsTest extends FlatSpec with BeforeAndAfter with SimpleTagTools with BeforeAndAfterEach
  with MockFactory with testData with ScalaFutures {

  override def beforeEach() {
    setupTestResources
  }

  override def afterEach() {
    teardownTestResources
  }

  "extractPodcastTags" should "return Tag for a file" in {
    whenReady(extractPodcastTagsFuture(audioFile1)) { result => result shouldBe defined }
  }

  it should "be None if Audio File is corrupt" in {
    whenReady(extractPodcastTagsFuture(audioFile2)) { result => result shouldBe None }
  }

  "isUnprocessedBBCPodcast" should "be true if the BBC were the composer and  the genre is not Podcast" in {
    whenReady(extractPodcastTagsFuture(audioFile3)) { result => isUnprocessedBBCPodcastTag(result.get) shouldBe true }
  }
  it should "be true with BBC's new format of composer 'BBC iPlayer' and genre is not Podcast " in {
    whenReady(extractPodcastTagsFuture(audioFileWithNewFormat1)) { result => isUnprocessedBBCPodcastTag(result.get) shouldBe true }
  }

  it should "be false if the BBC were the composer and  the genre is Podcast" in {
    whenReady(extractPodcastTagsFuture(audioFile6)) { result => isUnprocessedBBCPodcastTag(result.get) shouldBe false }
  }

  "isProcessedBBCPodcast" should "be false if the BBC were the composer and  the genre is not Podcast" in {
    whenReady(extractPodcastTagsFuture(audioFile3)) { result => isProcessedBBCPodcastTag(result.get) shouldBe false }
  }

  it should "be true if the BBC were the composer and  the genre is Podcast" in {
    whenReady(extractPodcastTagsFuture(audioFile6)) { result => isProcessedBBCPodcastTag(result.get) shouldBe true }
  }

  "renameField" should "be able to rename artist from artist: title format" in {
    whenReady(extractPodcastTagsFuture(audioFile7)) {
      result =>
        val list = List(RenamePattern(new Regex(": (.*)"), ""), RenamePattern(new Regex("\\A"), "("), RenamePattern(new Regex("\\z"), ")"))
        renameField(result.get, FieldKey.ARTIST, list).getFirst(FieldKey.ARTIST) shouldBe "(Monki)"
    }
  }
  it should "be able to rename title from artist: title format" in {
    whenReady(extractPodcastTagsFuture(audioFile7)) {
      result =>
        val list = List(RenamePattern(new Regex("(.*): "), ""))
        renameField(result.get, FieldKey.TITLE, list).getFirst(FieldKey.TITLE) shouldBe "Lee Foss Live Lights On Mix"
    }
  }
  it should "be able to rename album" in {
    whenReady(extractPodcastTagsFuture(audioFile7)) {
      result =>
        val list = List(RenamePattern(new Regex("\\A"), "(BBC Radio 1)-"))
        renameField(result.get, FieldKey.ALBUM, list).getFirst(FieldKey.ALBUM) shouldBe "(BBC Radio 1)-Monki"
    }
  }
  it should "be able to rename genre" in {
    whenReady(extractPodcastTagsFuture(audioFile7)) {
      result =>
        val list = List(RenamePattern(new Regex("[^\\n]+"), "Podcast"))
        renameField(result.get, FieldKey.GENRE, list).getFirst(FieldKey.GENRE) shouldBe "Podcast"
    }
  }

  "copyField" should "be able to copy from title to artist" in {
    whenReady(extractPodcastTagsFuture(audioFile7)) {
      result =>
        val newResult = copyField(result.get, FieldKey.TITLE, FieldKey.ARTIST)
        newResult.getFirst(FieldKey.ARTIST) shouldBe "Monki: Lee Foss Live Lights On Mix"
    }
  }

  "splitField" should "be able to split by : to get the artist" in {
    whenReady(extractPodcastTagsFuture(audioFileWithNewFormat2)) {
      result =>
        val newResult = splitField(result.get, FieldKey.ALBUM, ":", 0)
        newResult.getFirst(FieldKey.ALBUM) shouldBe "Chris Cornell"
    }
  }
  it should "be able to handle strings with no split character in it." in {
    whenReady(extractPodcastTagsFuture(audioFile7)) {
      result =>
        val newResult = splitField(result.get, FieldKey.ALBUM, ":", 0)
        newResult.getFirst(FieldKey.ALBUM) shouldBe "Monki"
    }
  }

  "stripIllegalCharacters" should "strip \\ from title name" in {
    stripIllegalCharacters("06\\02\\2017") shouldBe "06-02-2017"
  }
  it should "strip / from the title name" in {
    stripIllegalCharacters("06/02/2017") shouldBe "06-02-2017"
  }
  it should "strip : from the real title name" in {
    stripIllegalCharacters("(Friction)-Guest Mix: Ajd") shouldBe "(Friction)-Guest Mix Ajd"
  }

  "removeLeadingPunctuation" should "remove leading spaces" in {
    val testString = "  Jeff Mills Special"
    removeLeadingPunctuation(testString) shouldBe "Jeff Mills Special"
  }
}
