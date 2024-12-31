package aoc2024

import cats.effect.{IO, IOApp}
import part3Concurrency.Resources.getResourceFromFile
import utilsScala2.general.DebugWrapper

import java.io.File
import java.util.Scanner
import scala.concurrent.duration._

object Day9  extends IOApp.Simple {

  case class State(fileMap: List[Char]) {
    def compress(): List[Char] = {

      def splitRemainder(chars: List[Char]): (List[Char], List[Char]) = {
        def loop2(remain: List[Char], acc: List[Char]): (List[Char], List[Char]) = remain match {
          case Nil => (acc, remain)
          case h :: t if h == '.' => (acc, remain)
          case h :: t => loop2(t, h :: acc)
        }
        loop2(chars, List())
      }

      def loop(original: List[Char], processed: List[Char], remainder: List[Char]): List[Char] = original match {
        case Nil => processed
        case h :: t if h != '.' => {
          // we have a file part..we want to find first space in our file map and replace it
          val remainderParts = splitRemainder(remainder)
          loop(t, h :: remainderParts._1 ::: processed, remainderParts._2.tail)
        }
        case _ :: t => loop(t, processed, remainder)
      }
      loop(this.fileMap, List(), this.fileMap.reverse)
    }
  }

  def createFileMap(diskMap: List[Char]): List[Char] = {
    def loop(chars: List[Char], fileNo: Int, acc: List[Char], isSpace: Boolean): List[Char] = chars match {
      case Nil => acc
      case h :: t if isSpace => loop(t, fileNo, Range(0, h.asDigit).map(_ => '.').toList ::: acc, !isSpace)
      case h :: t => loop(t, fileNo + 1, Range(0, h.asDigit).map(_ => fileNo.toChar).toList ::: acc, !isSpace)
    }
    loop(diskMap, 0, List(), false)
  }
  def readLineByLine(scanner: Scanner, state: State): IO[Unit] = {
    if (scanner.hasNextLine) {
      for {
        l <- IO(scanner.nextLine()).debug1
        _ <- IO.sleep(1.millis)
        fileMap = createFileMap(l.toList)
        _ <- readLineByLine(scanner, state.copy(fileMap = fileMap))
      } yield ()
    }
    else {
      IO(s"final states is ...${state.compress}").debug1 *> IO.unit
    }
  }
  def resourceReadFile(path: String): IO[Unit] =
    IO(s"opening file at $path") *>
      getResourceFromFile(path).use {
        scanner =>
          readLineByLine(scanner, State(List[Char]()))
      }

  override def run: IO[Unit] = {
    val inputFile = new File("/Users/martinetherton/Developer/projects/be/scala/cats-course/src/main/resources/aoc2024/9.txt")
    resourceReadFile(inputFile.getAbsolutePath)
  }
}
