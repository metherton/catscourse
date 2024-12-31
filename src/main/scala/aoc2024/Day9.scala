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

      def findFirstSpace(fmp: List[Char]): Int = {
        def loop2(fileMap: List[Char], i: Int): Int = fileMap match {
          case Nil => i // if
          case h :: t if h == '.' => i
          case h :: t => loop2(t, i + 1)
        }
        loop2(fmp, 0)
      }

      def loop(originalReversed: List[Char], originalFileMap: List[Char], count: Int): List[Char] = originalReversed match {
        case Nil => originalFileMap
        case h :: t if h != '.' => {
          // we have a file part..we want to find first space in our file map and replace it
          val firstSpacePosition = findFirstSpace(originalFileMap)
          if (firstSpacePosition < originalFileMap.size) {
            val evenNewerFileMap = originalFileMap.updated(firstSpacePosition, h)
            loop(t, evenNewerFileMap, count + 1)
          } else {
            originalFileMap.take(originalFileMap.size - count) ::: Range(0, count).map(_ => '.').toList
          }

        }
        case _ :: t => loop(t, originalFileMap, count)
      }
      loop(this.fileMap, this.fileMap.reverse, 0)
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

  def checksum(m: List[Char]): Long = {
    def loop(chars: List[Char], acc: Long, i: Int): Long = chars match {
      case Nil => acc
      case h :: t => {
        println(s"$i: $acc")
        loop(t, if (h != '.') acc + (h.toLong * i) else acc, i + 1)
      }
    }
    loop(m, 0, 0)
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
      val result = checksum(state.compress)
      IO(s"final states is ...$result").debug1 *> IO.unit
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
