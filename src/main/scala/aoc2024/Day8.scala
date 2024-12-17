package aoc2024

import aoc2024.Day7.{Calculation, State, readLineByLine}
import cats.effect.{IO, IOApp}
import part3Concurrency.Resources.getResourceFromFile

import java.io.File
import java.util.Scanner
import utilsScala2.general.DebugWrapper
import scala.concurrent.duration._
object Day8  extends IOApp.Simple {

  case class State(antennas: List[Antenna])
  case class Coords(row: Int, col: Int)
  case class Antenna(id: Char, pos: Coords)

  def scanLine(char: List[Char], rowNo: Int): List[Antenna] = {
    def loop(s: List[Char], ants: List[Antenna], i: Int): List[Antenna] = s match {
      case Nil => ants
      case h :: t if h != '.' => loop(t, Antenna(h, Coords(row = rowNo, col = i)) :: ants, i + 1)
      case _ :: t => loop(t, ants, i + 1)
    }
    loop(char, List(), 0)
  }

  def readLineByLine(scanner: Scanner, state: State, rowNo: Int): IO[Unit] = {
    if (scanner.hasNextLine) {
      for {
        l <- IO(scanner.nextLine()).debug1
        _ <- IO.sleep(1.millis)
        ants = scanLine(l.toList, rowNo)
        _ <- readLineByLine(scanner, state.copy(antennas = ants ::: state.antennas), rowNo + 1)
      } yield ()
    }
    else {
      IO(s"final total is ...${state.antennas}").debug1 *> IO.unit
    }
  }
  def resourceReadFile(path: String): IO[Unit] =
    IO(s"opening file at $path") *>
      getResourceFromFile(path).use {
        scanner =>
          readLineByLine(scanner, State(List[Antenna]()), 0)
      }

  override def run: IO[Unit] = {
    val inputFile = new File("/Users/martinetherton/Developer/projects/be/scala/cats-course/src/main/resources/aoc2024/8.txt")
    resourceReadFile(inputFile.getAbsolutePath)
  }
}
