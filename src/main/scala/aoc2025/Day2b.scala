package aoc2025

import aoc2025.Day2b.findDups
import cats.effect.{IO, IOApp}
import part3Concurrency.Resources.getResourceFromFile
import utilsScala2.general.DebugWrapper

import java.io.File
import java.util.Scanner
import scala.concurrent.duration._

object Day2b extends IOApp.Simple {

  type Counter = Int
  type CurrentPosition = Int

  def findDups(tup: (String, String)): List[Long] = {
    val numsAndFactors = (for {
      num <- tup._1.toLong to tup._2.toLong
      len = num.toString.length
      factors = (1 to len).filter(len % _ == 0)
    } yield (num, factors)).toList

    val result = numsAndFactors.map(t => (for {
      x <- t._2.toList
      parts = t._1.toString.grouped(x).toList
      if parts.distinct.size == 1
     } yield t._1)).toList
     val r = result.filter(x => x.length > 1).map(x => x(0))
     r
  }

//    (for {
//      num <- tup._1.toLong to tup._2.toLong
//      len = num.toString.length
//      factors = (1 to len).filter(len % _ == 0)
//      i <- factors
//      parts = num.toString.grouped(i).toList
//      if parts.distinct.size == 1
//    } yield parts(0).toLong).toList

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
