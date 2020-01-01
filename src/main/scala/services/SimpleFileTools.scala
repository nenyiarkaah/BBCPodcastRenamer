package services

import domains.{PodSettings, PodcastItem}
import java.io.{BufferedWriter, File, FileWriter, FileNotFoundException, FileInputStream}

import org.apache.commons.io.{FileExistsException, FileUtils}

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.io.Source
import scala.util.{Failure, Success, Try}
import better.files.{File => ScalaFile, _}

/**
  * Created by Nenyi on 21/03/2017.
  */
class SimpleFileTools extends SimpleJson {
  def codeSource = getClass.getProtectionDomain().getCodeSource()

  def jarFile = new File(codeSource.getLocation().toURI().getPath())

  def jarDir = jarFile.getParentFile().getPath()

  def getCurrentDirectory = new File(jarDir).toString

  def getSeparator = File.separator

  val settingsPath = getCurrentDirectory + getSeparator + "settings.json"
  val defaultSettings = new PodSettings(getCurrentDirectory + getSeparator + "podcasts",
    getCurrentDirectory + getSeparator + "podcast", Array("m4a"))

  def createDestination(rootDestination: String, parentDestination: String) = rootDestination + getSeparator + parentDestination

  private def createSettingJson(filename: String): Try[String] = {
    val json = ConvertPodcastSettingsToJson(defaultSettings)
    Try {
      createFile(filename, json.toString)
    } match {
      case Failure(err) => Failure(err)
      case _ => Success(s"Created setting file $filename")
    }
  }

  private def openAndReadSettings(filename: String): String = {
    openAndReadFile(filename) match {
      case Failure(exception) =>
        exception match {
          case e: FileNotFoundException =>
            createSettingJson(filename)
            throw new FileNotFoundException("Settings file has just been created, please configure the source and destination paths")
          case _ => throw exception
        }
      case Success(filename) => filename
    }
  }

  def openAndReadSettings: PodSettings = ConvertJsonToPodcastSettings(openAndReadSettings(settingsPath))

  def openAndReadFile(filename: String): Try[String] = {
    Try(new FileInputStream(filename)) match {
      case Success(stream) => readFile(stream)
      case Failure(exception) => Failure(exception)
    }
  }

  private def readFile(stream: FileInputStream): Try[String] = {
    Try(Source.fromInputStream(stream).mkString)
  }

  private def createFile(filename: String, settings: String) = {
    val file = new File(filename)
    val bw = new BufferedWriter(new FileWriter(file))
    bw.write(settings)
    bw.close()
  }

  def getFiles(podSettings: PodSettings): List[File] = {
    val source = new File(podSettings.source)
    podSettings.extensions.flatMap(ext =>
      source.listFiles.filter(_.getName.endsWith(ext))).toList
  }

  def checkAndCreateParentDirectory(path: String) = {
    val dir = new File(path)
    if (!dir.exists) dir.mkdir()
  }

  def getListOfSubDirectories(directoryName: String): List[File] = {
    Try((new File(directoryName)).listFiles.filter(_.isDirectory).map(f => f).toList) match {
      case Success(subs) => subs
      case Failure(exception) => List()
    }
  }

  def getFileFolderSize(dir: File): Long = {
    dir.listFiles().map(_.length).sum
  }

  def convertToMB(size: Long): Long = {
    size / 1048576
  }

  def validSubDirectories(subDirectories: List[File], size: Long, limit: Long) = {
    subDirectories.filter(s => convertToMB(getFileFolderSize(s) + size) < limit)
  }

  def matchValidDir(subDirectories: List[File], validDirs: List[File], parent: String): File = validDirs match {
    case List() => createNewPodcastDestination(subDirectories, parent)
    case _ => validDirs.headOption match {
      case None => createNewPodcastDestination(subDirectories, parent)
      case Some(v) => v
    }
  }

  def move(source: String, destination: String) = {
    Try(FileUtils.moveFileToDirectory(
      FileUtils.getFile(source), FileUtils.getFile(destination), true)) match {
      case Success(f) => Success(destination)
      case Failure(ex) => ex match {
        case e: FileExistsException => Failure(new FileExistsException(s"Problem moving Podcast because it already exists: ${e.getMessage}"))
        case _ => Failure(new Exception(s"Problem moving Podcast: ${ex}"))
      }
    }
  }

  def createNewPodcastDestination(dirs: List[File], parent: String): File = {
    val file = dirs match {
      case List() => incrementAndCheckNewDirectory("", parent)
      case _ => incrementAndCheckNewDirectory(dirs.map(_.getName).sorted.last, dirs.map(_.getParent).sorted.last)
    }
    val dir = new File(file)
    dir.mkdir
    dir
  }

  def incrementAndCheckNewDirectory(folderId: String, parent: String): String = {
    checkAndCreateParentDirectory(parent)

    def checkFolderId(folderId: String): String = {
      folderId match {
        case id if isEmpty(id) =>
          checkAndCreateParentDirectory(parent + getSeparator + "0.1")
          println("creating folder " + parent + getSeparator + "0.1")
          "0.1"
        case id => increment(id, '.')
      }
    }

    parent + getSeparator + checkFolderId(folderId)
  }

  def isEmpty(str: String) = Option(str).forall(_.isEmpty)

  def increment(id: String, separator: Char): String = {
    def extractDigit(digit: Option[String], default: Int) = digit match {
      case None => default
      case Some(t) => t.toInt
    }

    val splitId = id.split(separator)
    val integer = extractDigit(splitId.headOption, 1)
    extractDigit(splitId.lastOption, 0) match {
      case 9 => (integer + 1).toString + separator + 0.toString
      case d => integer.toString + separator + (d + 1).toString
    }
  }

  /**
    * Get a recursive listing of all files underneath the given directory.
    * from stackoverflow.com/questions/2637643/how-do-i-list-all-files-in-a-subdirectory-in-scala
    */
  @deprecated
  def getRecursiveListOfFilesSeq(dir: File, extensions: Array[String]): Array[File] = {
    dir.exists match {
      case false => Array[File]()
      case true =>
        val betterDir = ScalaFile(dir.getAbsolutePath)
        val files = betterDir.listRecursively.filterNot(_.isDirectory).toList
        extensions.flatMap(ext => files.filter(_.extension(false,false,true) == Some(ext))).map(_.toJava)
    }
  }

  def getRecursiveListOfFilesByFutures(dir: File, extensions: Array[String]): Future[Array[File]] = Future {
    dir.exists match {
      case false => Array[File]()
      case true =>
        val betterDir = ScalaFile(dir.getAbsolutePath)
        val files = betterDir.listRecursively.filterNot(_.isDirectory).toList
        extensions.flatMap(ext => files.filter(_.extension(false,false,true) == Some(ext))).map(_.toJava)
    }
  }

  def doesDirectoryExist(dir: String, message: String) = {
    val directory = new File(dir)
    if(!directory.exists) throw new FileNotFoundException(message)
  }

}

