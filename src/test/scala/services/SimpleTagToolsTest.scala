package services

import java.io.File

import domains._
import org.jaudiotagger.tag.{FieldKey, Tag, TagField}
import org.scalamock.scalatest.MockFactory
import org.scalatest.Matchers._
import org.scalatest.{BeforeAndAfter, FlatSpec}
import org.scalatest.concurrent.ScalaFutures

import scala.util.matching.Regex

/**
  * Created by Nenyi on 24/03/2017.
  */
class SimpleTagToolsTest extends FlatSpec with BeforeAndAfter with SimpleTagTools with MockFactory with testData with ScalaFutures {

  def beforeEach() {

    setupTestResources
  }

  def afterEach() {
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

  "stripIllegalCharacters" should "strip \\ from title name" in {
    stripIllegalCharacters("06\\02\\2017") shouldBe "06-02-2017"
  }
  it should "strip / from the title name" in {
    stripIllegalCharacters("06/02/2017") shouldBe "06-02-2017"
  }

}
