package aoc2025

import aoc2025.Day3a.State
import cats.effect.{IO, IOApp}
import part3Concurrency.Resources.getResourceFromFile
import utilsScala2.general.DebugWrapper

import java.io.File
import java.util.Scanner
import scala.concurrent.duration._

object Day3a extends IOApp.Simple {

  case class Pair(highest: Int, nextHighest: Int)
  case class State(st: List[Pair]) {
    def getTotal(): Int = {
      st.map(p => (p.highest.toString + p.nextHighest.toString).toInt).sum
    }

  }
  def calcPairs(line: String): Pair = {
    def loop(rem: List[Int], p: Pair): Pair = rem match {
      case Nil => p
      case h1 :: Nil if (h1 > p.nextHighest) => loop(Nil, Pair(p.highest, h1))
      case h1 :: h2 :: Nil if (h1 > p.highest) => loop(Nil, Pair(h1, h2))
      case h1 :: t if (h1 > p.highest) => loop(t, Pair(h1, 0))
      case h1 :: t if (h1 > p.nextHighest) => loop(t, Pair(p.highest, h1))
      case _ :: t => loop(t, p)
    }
    loop(line.toList.drop(2).map(_.asDigit),Pair(line.charAt(0).asDigit, line.charAt(1).asDigit))
  }

  def readLineByLine(scanner: Scanner, state: State): IO[Unit] = {
    if (scanner.hasNextLine) {
      for {
        l <- IO(scanner.nextLine()).debug1
        _ <- IO.sleep(1.millis)
        _ <- readLineByLine(scanner, state.copy(calcPairs(l) :: state.st))
      } yield ()
    }
    else {
      IO(s"final total is ...${state.getTotal}").debug1 *> IO.unit
    }
  }
  def resourceReadFile(path: String): IO[Unit] =
    IO(s"opening file at $path") *>
      getResourceFromFile(path).use {
        scanner =>
          readLineByLine(scanner, State(List()))
      }
  override def run: IO[Unit] = {
    val inputFile = new File("/Users/martinetherton/Developer/projects/be/scala/cats-course/src/main/resources/aoc2025/3.txt")
    resourceReadFile(inputFile.getAbsolutePath)
  }
}
