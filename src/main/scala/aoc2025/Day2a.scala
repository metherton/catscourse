package aoc2025

import cats.effect.{IO, IOApp}
import part3Concurrency.Resources.getResourceFromFile
import utilsScala2.general.DebugWrapper

import java.io.File
import java.util.Scanner
import scala.concurrent.duration._
import scala.math.abs

object Day2a extends IOApp.Simple {

  type Counter = Int
  type CurrentPosition = Int

  def findDups(tup: (String, String)): List[Long] = {
    (for {
      num <- tup._1.toLong to tup._2.toLong
      twoParts = num.toString.splitAt(num.toString.length/2)
      if twoParts._1.equals(twoParts._2)
    } yield num).toList
  }
  def calcInvalidIds(l: String): List[Long] = {
    val idsRange = l.split(",").toList
    (for {
      rg <- idsRange
      pairList = rg.split("-").toList
      pairTup = (pairList(0), pairList(1))
      dups = findDups(pairTup)
    } yield dups).flatten
  }
  case class State(invalidIds: List[Long]) {
    def getSum: Long = invalidIds.map(_.toLong).sum
  }
  def readLineByLine(scanner: Scanner, state: State): IO[Unit] = {
    if (scanner.hasNextLine) {
      for {
        l <- IO(scanner.nextLine()).debug1
        _ <- IO.sleep(1.millis)
        _ <- readLineByLine(scanner, state.copy(invalidIds = calcInvalidIds(l) ::: state.invalidIds))
      } yield ()
    }
    else {
      IO(s"final total is ...${state.getSum}").debug1 *> IO.unit
    }
  }
  def resourceReadFile(path: String): IO[Unit] =
    IO(s"opening file at $path") *>
      getResourceFromFile(path).use {
        scanner =>
          readLineByLine(scanner, State(List()))
      }
  override def run: IO[Unit] = {
    val inputFile = new File("/Users/martinetherton/Developer/projects/be/scala/cats-course/src/main/resources/aoc2025/2.txt")
    resourceReadFile(inputFile.getAbsolutePath)
  }
}
