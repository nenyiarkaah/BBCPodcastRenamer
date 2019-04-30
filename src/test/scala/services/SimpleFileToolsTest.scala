package services

import java.io.{File, FileNotFoundException}

import domains.PodSettings
import org.apache.commons.io.{FileExistsException, FilenameUtils}
import org.scalatest.Matchers._
import org.scalatest.{BeforeAndAfter, BeforeAndAfterEach, FlatSpec}
import org.scalatest.PrivateMethodTester._
import org.scalamock.scalatest.MockFactory
import org.scalatest.concurrent.ScalaFutures

import scala.util.{Failure, Try}

/**
  * Created by Nenyi on 21/03/2017.
  */
class SimpleFileToolsTest extends FlatSpec with BeforeAndAfter with BeforeAndAfterEach
  with MockFactory with testData with ScalaFutures {
  val numberOfDestinationFiles = 13
  val numberOfDestinationSubDirectories = 4
  val numberOfDestinationSubDirectoriesUnderTestSize = 4
  val nonExistantDirectory = new File(destination + "random")
  var s: SimpleFileTools = _

  before {
    s = new SimpleFileTools
    deleteSettings(s settingsPath)
  }

  override def beforeEach {
    setupTestResources
  }

  override def afterEach {
    teardownTestResources
    deleteSettings(s settingsPath)
  }

  def deleteSettings(settingsPath: String) = {
    new File(settingsPath).delete
  }

  "openAndReadSettings" should "throw an exception if there is no settings file" in {
    intercept[FileNotFoundException](s openAndReadSettings)
  }
  it should "should produce a PodcastSettings" in {
    val thePrivateMethod = PrivateMethod[Try[String]]('createSettingJson)
    s invokePrivate thePrivateMethod(s settingsPath)
    s.openAndReadSettings.isInstanceOf[PodSettings] shouldBe true
  }
  "createSettingJson" should "create a settings file" in {
    val thePrivateMethod = PrivateMethod[Try[String]]('createSettingJson)
    val result = s invokePrivate thePrivateMethod(s settingsPath)
    result.isSuccess shouldEqual true
  }
  "openAndReadFile" should "throw an exception if there is no file" in {
    s.openAndReadFile(s settingsPath).isFailure shouldBe true
  }
  "getFiles" should "only bring back files with the correct extension" in {
    val files = s getFiles (resourceSettings)
    files.size should be > 0
    for (file <- files) {
      resourceSettings.extensions should contain(FilenameUtils.getExtension(file.getAbsolutePath))
    }
  }

  "checkAndCreateParentDirectory" should "" in {
    val randomPath = resourceSettings.destination + s.getSeparator + "randomfolder"
    new File(randomPath).exists() shouldBe false
    s checkAndCreateParentDirectory (randomPath)
    new File(randomPath).exists() shouldBe true
    deleteSettings(randomPath)
  }


  "getListOfSubDirectories" should "be able to count the 2 subfolders in the destination folder" in {
    val result = s getListOfSubDirectories (destination)
    result.length shouldEqual numberOfDestinationSubDirectories
  }

  "getFileFolderSize" should "get the folder size of the resource folder" in {
    val dest = new File(destinationPodcast)
    assert(1809915 to 1811215 contains s.getFileFolderSize(dest))
  }

  "convertToMB" should "convert 121415259 bytes to 115 MB" in {
    s convertToMB (121415259) shouldBe 115
  }

  "validSubDirectories" should "return all 2 folders and podcast under the size of 4140 MB" in {
    val subFolders = new File(destination).listFiles.filter(_.isDirectory).toList
    val size = audioFile3.length
    val results = s validSubDirectories(subFolders, size, 4140)
    results.length shouldBe numberOfDestinationSubDirectories
  }
  it should "return all 1 folders and podcast under the size of 117 MB" in {
    val subFolders = new File(destination).listFiles.filter(_.isDirectory).toList
    val size = audioFile3.length
    val results = s validSubDirectories(subFolders, size, 117)
    results.length shouldBe numberOfDestinationSubDirectoriesUnderTestSize
  }
  it should "be able to return empty folder as valid" in {
    val subFolders = new File(destination).listFiles.filter(_.isDirectory).toList
    val size = audioFile3.length
    val results = s validSubDirectories(subFolders, size, 116)
    results.length shouldBe numberOfDestinationSubDirectoriesUnderTestSize
  }

  "increment" should "increment 1.0 to 1.1" in {
    val parent = "1.0"
    s increment(parent, '.') shouldBe "1.1"
  }
  it should "increment 1.1 to 1.2" in {
    val parent = "1.1"
    s increment(parent, '.') shouldBe "1.2"
  }

  "incrementAndCheckNewDirectory" should "increment from empty to 0.1" in {
    s incrementAndCheckNewDirectory("", destinationPodcast) shouldBe destinationPodcast + "/0.1"
  }

  "createNewPodcastDestination" should "create a new 0.1 directory within parent directory" in {
    val result = s createNewPodcastDestination(List(), destinationPodcast)
    result.exists shouldBe true
    result.getAbsolutePath shouldBe destinationPodcast + s.getSeparator + "0.1"
    result.delete
  }
  it should "create a new 1.0 directory within parent directory" in {
    val folder = new File(destinationPodcast + s.getSeparator + "1.0")
    folder.mkdir
    val result = s createNewPodcastDestination(List(folder), destinationPodcast)
    result.exists shouldBe true
    result.getAbsolutePath shouldBe destinationPodcast + s.getSeparator + "1.1"
    result.delete
    folder.delete
  }

  "matchValidDir" should "return a valid sub directory of 0.1" in {
    val result = s matchValidDir(List(), List(), destinationPodcast)
    result.exists shouldBe true
    result.getAbsolutePath shouldBe destinationPodcast + s.getSeparator + "0.1"
    result.delete
  }

  it should "return a valid sub directory of 1.1 no valid directories" in {
    val folder = new File(destinationPodcast + s.getSeparator + "1.0")
    folder.mkdir
    val result = s matchValidDir(List(folder), List(), destinationPodcast)
    result.exists shouldBe true
    result.getAbsolutePath shouldBe destinationPodcast + s.getSeparator + "1.1"
    result.delete
    folder.delete
  }

  it should "return a valid sub directory of 1.1 when 1.1 valid directories" in {
    val folder = new File(destinationPodcast + s.getSeparator + "1.0")
    folder.mkdir
    val validFolder = new File(destinationPodcast + s.getSeparator + "1.1")
    validFolder.mkdir
    val result = s matchValidDir(List(folder, validFolder), List(validFolder), destinationPodcast)
    result.exists shouldBe true
    result.getAbsolutePath shouldBe destinationPodcast + s.getSeparator + "1.1"
    result.delete
    folder.delete
  }
  it should "return a valid sub directory of 1.2 single valid directory" in {
    val folder = new File(destinationPodcast + s.getSeparator + "1.0")
    folder.mkdir
    val folder2 = new File(destinationPodcast + s.getSeparator + "1.1")
    folder2.mkdir
    val validFolder = new File(destinationPodcast + s.getSeparator + "1.2")
    validFolder.mkdir
    val result = s matchValidDir(List(folder, folder2, validFolder), List(validFolder), destinationPodcast)
    result.exists shouldBe true
    result.getAbsolutePath shouldBe destinationPodcast + s.getSeparator + "1.2"
    result.delete
    folder.delete
    folder2.delete
  }
  it should "return a valid sub directory of 1.1 multiple valid directory" in {
    val folder = new File(destinationPodcast + s.getSeparator + "1.0")
    folder.mkdir
    val validFolder = new File(destinationPodcast + s.getSeparator + "1.1")
    validFolder.mkdir
    val validFolder2 = new File(destinationPodcast + s.getSeparator + "1.2")
    validFolder2.mkdir
    val result = s matchValidDir(List(folder, validFolder, validFolder2), List(validFolder, validFolder2), destinationPodcast)
    result.exists shouldBe true
    result.getAbsolutePath shouldBe destinationPodcast + s.getSeparator + "1.1"
    result.delete
    folder.delete
    validFolder.delete
    validFolder2.delete
  }

  "move" should "move a file to a destination directory" in {
    val filePath = audioFile2.getAbsolutePath
    val filename = audioFile2.getName
    val destinationFile = destinationPodcast + getSeparator + filename
    val parent = audioFile2.getParent
    val result = s move(filePath, destinationPodcast)
    result.isSuccess shouldBe true
    s move(destinationFile, parent)
  }
  it should "throw an exception if a file already exists" in {
    val filePath = audioFile1.getAbsolutePath
    val result = s move(filePath, destinationPodcast)
    result.isFailure shouldBe true
    val ex = result match {
      case Failure(ex) => ex
    }
    ex.isInstanceOf[FileExistsException] shouldBe true
  }


  "getRecursiveListOfFiles" should "able get sub directory files from a directory" in {
    val results = s getRecursiveListOfFilesSeq(new File(destination), defaultSettings.extensions)
    results.size shouldBe numberOfDestinationFiles
  }
  it should "return an empty list if the directory does not exist" in {
    val results = s getRecursiveListOfFilesSeq(nonExistantDirectory, defaultSettings.extensions)
    results.size shouldBe 0
  }

  "getRecursiveListOfFilesByFutures" should "able get sub directory files from a directory" in {
      whenReady(s getRecursiveListOfFilesByFutures(new File(destination), defaultSettings.extensions)) {
        results =>
          results.size shouldBe numberOfDestinationFiles
      }
  }
  it should "return an empty list if the directory does not exist" in {
    whenReady(s getRecursiveListOfFilesByFutures(nonExistantDirectory, defaultSettings.extensions)) {
      results =>
        results.size shouldBe 0
    }
  }

}
